use crate::parser::ParseConfig;

#[derive(Debug, Copy, Clone)]
pub(crate) enum Event<C: ParseConfig> {
    StartNode { kind: C::NodeKind },
    FinishNode,
    AddToken,
}
