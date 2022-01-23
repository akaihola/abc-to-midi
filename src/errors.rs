use std::{
    error::{self, Error},
    fmt,
};

pub type Result<T> = std::result::Result<T, Box<dyn error::Error>>;

#[derive(Debug)]
pub struct PitchConversionError;

impl fmt::Display for PitchConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Can't convert this type to a note")
    }
}

impl Error for PitchConversionError {}

#[derive(Debug)]
pub struct InfoFieldMissing(pub char);

impl fmt::Display for InfoFieldMissing {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "The info field {}: is missing", self.0)
    }
}

impl Error for InfoFieldMissing {}
