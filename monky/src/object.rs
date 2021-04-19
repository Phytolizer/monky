use std::fmt::Debug;
use std::fmt::Display;

pub enum ObjectKind {
    Integer,
    Boolean,
    Null,
}

pub trait Object: Debug + Display {
    fn kind(&self) -> ObjectKind;
    fn inspect(&self) -> String;
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl Display for Integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Object for Integer {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Integer
    }

    fn inspect(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Object for Boolean {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Boolean
    }

    fn inspect(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug)]
pub struct Null;

impl Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

impl Object for Null {
    fn kind(&self) -> ObjectKind {
        ObjectKind::Null
    }

    fn inspect(&self) -> String {
        self.to_string()
    }
}
