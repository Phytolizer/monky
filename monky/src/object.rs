use monky_test_macros::IsAs;

use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum ObjectKind {
    Integer,
    Boolean,
    Null,
}

#[derive(Debug, PartialEq, IsAs)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Null,
}

pub trait Object: Debug + Display {
    fn kind(&self) -> ObjectKind;
    fn inspect(&self) -> String;
    fn value(&self) -> Value;
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

    fn value(&self) -> Value {
        Value::Integer(self.value)
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

    fn value(&self) -> Value {
        Value::Boolean(self.value)
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

    fn value(&self) -> Value {
        Value::Null
    }
}
