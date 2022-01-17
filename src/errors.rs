use std::{error::Error, fmt};

#[derive(Debug)]
pub struct PitchConversionError;

impl fmt::Display for PitchConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Can't convert this type to a note")
    }
}

impl Error for PitchConversionError {}
