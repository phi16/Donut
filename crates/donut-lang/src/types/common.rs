#[derive(Debug, Clone)]
pub struct TokenPos {
    pub line: usize,
    pub col: usize,
    pub len: usize,
}

pub type Error = (TokenPos, String);

#[derive(Debug, Clone)]
pub struct TokenSpan {
    pub start: usize,
    pub end: usize,
}

// annotated node
#[derive(Debug)]
pub enum A<T> {
    Accepted(T, TokenSpan),
    Error(),
}

impl<T> A<T> {
    pub fn inner(&self) -> Option<&T> {
        match self {
            A::Accepted(v, _) => Some(v),
            A::Error() => None,
        }
    }

    pub fn accepted(&self) -> Option<(&T, &TokenSpan)> {
        match self {
            A::Accepted(v, span) => Some((v, span)),
            A::Error() => None,
        }
    }
}
