use drop_bomb::DropBomb;

pub struct Marker {
    pub(crate) index: usize,
    bomb: DropBomb,
}

pub struct CompletedMarker {
    pub(crate) index: usize,
}

impl Marker {
    pub(crate) fn new(index: usize) -> Self {
        Self {
            index,
            bomb: DropBomb::new("markers must be completed"),
        }
    }

    pub(crate) fn complete(mut self) -> CompletedMarker {
        self.bomb.defuse();
        CompletedMarker { index: self.index }
    }
}
