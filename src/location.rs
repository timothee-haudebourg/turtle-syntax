use std::convert::AsRef;
use std::ops::{Deref, DerefMut};
use std::hash::{Hash, Hasher};
use std::fmt;
use source_span::Span;

pub struct Loc<T> {
    span: Span,
    value: T
}

impl<T> Loc<T> {
    pub fn new(t: T, span: Span) -> Loc<T> {
        Loc {
            span: span,
            value: t
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_inner(self) -> T {
        self.value
    }

    pub fn into_raw_parts(self) -> (T, Span) {
        (self.value, self.span)
    }
}

impl<T: Clone> Clone for Loc<T> {
    fn clone(&self) -> Loc<T> {
        Loc {
            span: self.span,
            value: self.value.clone()
        }
    }
}

impl<T> AsRef<T> for Loc<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}


impl<T: fmt::Display> fmt::Display for Loc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::Debug> fmt::Debug for Loc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: Hash> Hash for Loc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.value.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Loc<T> {
    fn eq(&self, other: &Loc<T>) -> bool {
        self.span == other.span && self.value == other.value
    }
}

impl<T: Eq> Eq for Loc<T> {}

impl<T> Deref for Loc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Loc<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}
