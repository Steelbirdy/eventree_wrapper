use crate::parser::{
    event::Event, sink::Sink, CompletedMarker, Marker, ParseConfig, ParseError, ParseResult,
    TokenSet, Tokens,
};
use eventree::TextRange;
use std::cell::Cell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

const NO_EXPECTED_MESSAGE: &str = "no expected syntax was set. Use `Parser::expected` to specify \
what the parser expected before using `Parser::error`";

pub struct Parser<C, T>
where
    C: ParseConfig,
    C::TokenKind: Copy + PartialEq,
    T: Tokens<TokenKind = C::TokenKind>,
{
    tokens: T,
    events: Vec<Option<Event<C>>>,
    errors: Vec<ParseError<C>>,
    token_idx: usize,
    expected: Option<ExpectedKind<C>>,
    expected_state: Rc<Cell<ExpectedState>>,
}

impl<C, T> Parser<C, T>
where
    C: ParseConfig,
    C::TokenKind: Copy + PartialEq,
    C::NodeKind: Copy,
    T: Tokens<TokenKind = C::TokenKind>,
{
    /// # Panics
    /// This is here to satisfy clippy until I get around to writing proper docs
    pub fn parse<G>(source: &str, tokens: T, grammar: G) -> ParseResult<C>
    where
        G: FnOnce(&mut Self),
    {
        let mut this = Self {
            tokens,
            token_idx: 0,
            events: Vec::new(),
            errors: Vec::new(),
            expected: None,
            expected_state: Rc::default(),
        };
        grammar(&mut this);

        // Since `Parser` can only be mutably accessed by user code in `grammar`,
        // failing to complete markers will result in runtime panics before here.
        // Hence if we get this far, all events are populated.
        // TODO: Is it possible for a Parser to create another Parser and mess with its events?
        assert!(this.events.iter().all(Option::is_some));

        // See https://stackoverflow.com/a/55081958
        let events = unsafe {
            let mut copied = std::mem::ManuallyDrop::new(this.events);
            Vec::from_raw_parts(
                copied.as_mut_ptr().cast::<Event<C>>(),
                copied.len(),
                copied.capacity(),
            )
        };
        Sink::new(events, source, this.tokens).finish(this.errors)
    }

    pub fn is_at(&mut self, kind: C::TokenKind) -> bool {
        if let ExpectedState::Unnamed = self.expected_state.get() {
            match &mut self.expected {
                Some(expected) => expected.add(kind),
                x @ None => {
                    *x = Some(ExpectedKind::Unnamed(kind));
                }
            }
        }
        self.skip_trivia();
        self.is_at_raw(kind)
    }

    pub fn is_at_any(&mut self, set: TokenSet<C::TokenKind>) -> bool {
        if let ExpectedState::Unnamed = self.expected_state.get() {
            if !set.is_empty() {
                self.expected
                    .get_or_insert(ExpectedKind::AnyUnnamed(TokenSet::EMPTY))
                    .add_all(set);
            }
        }

        self.skip_trivia();
        self.peek().map_or(false, |kind| set.contains(kind))
    }

    pub fn is_at_end(&mut self) -> bool {
        self.skip_trivia();
        self.tokens.is_at_end(self.token_idx)
    }

    pub fn expect(&mut self, kind: C::TokenKind) {
        self.expect_with_recovery_set(kind, TokenSet::EMPTY);
    }

    pub fn expect_any(&mut self, set: TokenSet<C::TokenKind>) {
        self.expect_any_with_recovery_set(set, TokenSet::EMPTY);
    }

    pub fn expect_with_recovery_set(
        &mut self,
        kind: C::TokenKind,
        recovery_set: TokenSet<C::TokenKind>,
    ) {
        if self.is_at(kind) {
            self.bump();
        } else {
            self.error_with_recovery_set(recovery_set);
        }
    }

    pub fn expect_any_with_recovery_set(
        &mut self,
        set: TokenSet<C::TokenKind>,
        recovery_set: TokenSet<C::TokenKind>,
    ) {
        if self.is_at_any(set) {
            self.bump();
        } else {
            self.error_with_recovery_set(recovery_set);
        }
    }

    pub fn expect_without_skipping(&mut self, kind: C::TokenKind) {
        if self.is_at(kind) {
            self.bump();
        } else {
            self.error_without_skipping();
        }
    }

    pub fn custom_error(&mut self, error: C::Error) {
        self.clear_expected();
        self.errors.push(ParseError::User(error));
    }

    pub fn error(&mut self) -> Option<CompletedMarker> {
        self.error_with_recovery_set(TokenSet::EMPTY)
    }

    pub fn error_with_recovery_set(
        &mut self,
        recovery_set: TokenSet<C::TokenKind>,
    ) -> Option<CompletedMarker> {
        self.error_with_only_recovery_set(recovery_set.union(C::default_recovery_set()))
    }

    pub fn error_without_skipping(&mut self) {
        let expected = self.clear_expected().expect(NO_EXPECTED_MESSAGE);
        let range = self.previous_range();
        self.errors.push(ParseError::Missing {
            expected,
            offset: range.end(),
        });
    }

    pub fn error_with_only_recovery_set(
        &mut self,
        recovery_set: TokenSet<C::TokenKind>,
    ) -> Option<CompletedMarker> {
        let expected = self.clear_expected().expect(NO_EXPECTED_MESSAGE);
        if self.is_at_end() || self.is_at_any_raw(recovery_set) {
            let range = self.previous_range();
            self.errors.push(ParseError::Missing {
                expected,
                offset: range.end(),
            });
            return None;
        }

        self.errors.push(ParseError::Unexpected {
            expected,
            found: self.tokens.kind(self.token_idx),
            range: self.tokens.range(self.token_idx),
        });

        let marker = self.start();
        self.bump();
        Some(self.complete(marker, C::ERROR_NODE_KIND))
    }

    #[must_use = "the drop guard must stay in scope to be useful. Try `let _guard = ...`"]
    pub fn expected(&mut self, name: &'static str) -> ExpectedDropGuard {
        self.expected_state.set(ExpectedState::Named);
        self.expected = Some(ExpectedKind::Named(name));
        ExpectedDropGuard {
            state: Rc::clone(&self.expected_state),
        }
    }

    pub fn bump(&mut self) {
        self.clear_expected();
        self.events.push(Some(Event::AddToken));
        self.token_idx += 1;
    }

    #[must_use = "markers must be completed before they are dropped"]
    pub fn start(&mut self) -> Marker {
        let index = self.events.len();
        self.events.push(None);
        Marker::new(index)
    }

    /// # Panics
    /// This is here to satisfy clippy until I get around to writing proper docs
    pub fn complete(&mut self, marker: Marker, kind: C::NodeKind) -> CompletedMarker {
        let index = marker.index;
        let old_event = std::mem::replace(&mut self.events[index], Some(Event::StartNode { kind }));
        assert!(old_event.is_none());
        self.events.push(Some(Event::FinishNode));
        marker.complete()
    }

    #[allow(clippy::needless_pass_by_value)]
    #[must_use = "markers must be completed before they are dropped"]
    pub fn precede(&mut self, marker: CompletedMarker) -> Marker {
        self.events.insert(marker.index, None);
        Marker::new(marker.index)
    }

    pub fn peek(&mut self) -> Option<C::TokenKind> {
        self.skip_trivia();
        self.peek_raw()
    }

    pub fn peek_range(&mut self) -> Option<TextRange> {
        self.skip_trivia();
        self.peek_range_raw()
    }

    pub fn previous_range(&self) -> TextRange {
        if self.token_idx == 0 {
            return TextRange::empty(0.into());
        }
        let mut prev_idx = self.token_idx - 1;
        while C::is_trivia(&self.tokens.kind(prev_idx)) {
            prev_idx -= 1;
        }
        self.tokens.range(prev_idx)
    }

    fn skip_trivia(&mut self) {
        while self.peek_raw().filter(C::is_trivia).is_some() {
            self.token_idx += 1;
        }
    }

    fn is_at_raw(&self, kind: C::TokenKind) -> bool {
        self.peek_raw() == Some(kind)
    }

    fn is_at_any_raw(&self, set: TokenSet<C::TokenKind>) -> bool {
        self.peek_raw().map_or(false, |tok| set.contains(tok))
    }

    fn peek_raw(&self) -> Option<C::TokenKind> {
        self.tokens.get_kind(self.token_idx)
    }

    fn peek_range_raw(&self) -> Option<TextRange> {
        self.tokens.get_range(self.token_idx)
    }

    fn clear_expected(&mut self) -> Option<ExpectedKind<C>> {
        let ret = self.expected.take();
        self.expected_state.set(ExpectedState::Unnamed);
        ret
    }
}

pub enum ExpectedKind<C: ParseConfig> {
    Named(&'static str),
    Unnamed(C::TokenKind),
    AnyUnnamed(TokenSet<C::TokenKind>),
}

impl<C: ParseConfig> ExpectedKind<C>
where
    C::TokenKind: Copy + PartialEq,
{
    fn add(&mut self, kind: C::TokenKind) {
        match self {
            Self::Named(_) => panic!("cannot add a TokenKind to a named ExpectedKind"),
            Self::Unnamed(prev) if *prev == kind => {}
            Self::Unnamed(prev) => {
                let set = TokenSet::new([kind, *prev]);
                *self = Self::AnyUnnamed(set);
            }
            Self::AnyUnnamed(set) => set.insert(kind),
        }
    }

    fn add_all(&mut self, set: TokenSet<C::TokenKind>) {
        match self {
            Self::Named(_) => panic!("cannot add TokenKinds to a named ExpectedKind"),
            Self::Unnamed(prev) => {
                *self = Self::AnyUnnamed(set.with(*prev));
            }
            Self::AnyUnnamed(prev) => {
                *prev = prev.union(set);
            }
        }
    }
}

impl<C: ParseConfig> Copy for ExpectedKind<C> where C::TokenKind: Copy {}

impl<C: ParseConfig> Clone for ExpectedKind<C>
where
    C::TokenKind: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Named(name) => Self::Named(name),
            Self::Unnamed(kind) => Self::Unnamed(kind.clone()),
            Self::AnyUnnamed(set) => Self::AnyUnnamed(*set),
        }
    }
}

impl<C: ParseConfig> Eq for ExpectedKind<C> where C::TokenKind: Eq {}

// TODO: Should the case (AnyUnnamed([kind]), Unnamed(kind)) be considered equal?
impl<C: ParseConfig> PartialEq for ExpectedKind<C>
where
    C::TokenKind: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Named(a), Self::Named(b)) => a == b,
            (Self::Unnamed(a), Self::Unnamed(b)) => a == b,
            (Self::AnyUnnamed(a), Self::AnyUnnamed(b)) => a == b,
            _ => false,
        }
    }
}

impl<C: ParseConfig> Hash for ExpectedKind<C>
where
    C::TokenKind: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        let discrim = std::mem::discriminant(self);
        discrim.hash(state);

        match self {
            Self::Named(name) => name.hash(state),
            Self::Unnamed(kind) => kind.hash(state),
            Self::AnyUnnamed(set) => set.hash(state),
        }
    }
}

impl<C: ParseConfig> fmt::Debug for ExpectedKind<C>
where
    TokenSet<C::TokenKind>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(name) => f.debug_tuple("Named").field(name).finish(),
            Self::Unnamed(kind) => f.debug_tuple("Unnamed").field(kind).finish(),
            Self::AnyUnnamed(set) => f.debug_tuple("AnyUnnamed").field(set).finish(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum ExpectedState {
    Named,
    Unnamed,
}

impl Default for ExpectedState {
    fn default() -> Self {
        Self::Unnamed
    }
}

pub struct ExpectedDropGuard {
    state: Rc<Cell<ExpectedState>>,
}

impl Drop for ExpectedDropGuard {
    fn drop(&mut self) {
        self.state.set(ExpectedState::Unnamed);
    }
}
