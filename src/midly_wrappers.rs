use midly::{self, num::u28};
use std::ops::Deref;

#[derive(Debug, PartialEq)]
pub struct MidiMessage(pub midly::MidiMessage);

pub struct Track<'a>(pub midly::Track<'a>);

impl<'a> Deref for Track<'a> {
    type Target = midly::Track<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Track<'_> {
    pub fn time_into_ticks(time: f32) -> u28 {
        u28::from((10000.0 * time) as u32)
    }
}
