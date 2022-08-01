use crate::parser::{
    event::Event,
    ParseError, ParseResult, {ParseConfig, Tokens},
};

pub(crate) struct Sink<C, T>
where
    C: ParseConfig,
    T: Tokens<TokenKind = C::TokenKind>,
{
    events: Vec<Event<C>>,
    tokens: T,
    token_idx: usize,
    builder: eventree::SyntaxBuilder<C>,
}

impl<C, T> Sink<C, T>
where
    C: ParseConfig,
    C::NodeKind: Copy,
    T: Tokens<TokenKind = C::TokenKind>,
{
    pub(crate) fn new(events: Vec<Event<C>>, source: &str, tokens: T) -> Self {
        Self {
            events,
            tokens,
            token_idx: 0,
            builder: eventree::SyntaxBuilder::new(source),
        }
    }

    pub(crate) fn finish(mut self, errors: Vec<ParseError<C>>) -> ParseResult<C> {
        assert!(matches!(self.events.first(), Some(Event::StartNode { .. })));
        assert!(matches!(self.events.last(), Some(Event::FinishNode)));

        let events = std::mem::take(&mut self.events);
        let iter = events.windows(2).map(|w| match w {
            [current, next] => (current, next),
            _ => unreachable!(),
        });

        for (current, next) in iter {
            self.process_event(*current);
            if matches!(next, Event::StartNode { .. } | Event::AddToken) {
                self.skip_trivia();
            }
        }

        // unconditionally skip any trivia before processing the last event
        // to ensure we don't miss trailing trivia at the end of the input
        self.skip_trivia();
        let last = events.last().copied().unwrap();
        self.process_event(last);

        ParseResult::new(self.builder.finish(), errors)
    }

    fn process_event(&mut self, event: Event<C>) {
        match event {
            Event::StartNode { kind } => {
                self.builder.start_node(kind);
            }
            Event::FinishNode => {
                self.builder.finish_node();
            }
            Event::AddToken => {
                self.add_token();
            }
        }
    }

    fn skip_trivia(&mut self) {
        loop {
            let is_trivia = self
                .tokens
                .get_kind(self.token_idx)
                .filter(C::is_trivia)
                .is_some();
            if !is_trivia {
                break;
            }
            self.add_token();
        }
    }

    fn add_token(&mut self) {
        let kind = self.tokens.kind(self.token_idx);
        let range = self.tokens.range(self.token_idx);
        self.builder.add_token(kind, range);
        self.token_idx += 1;
    }
}
