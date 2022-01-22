use midly::{self, num::u28, Header};
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
        u28::from((240.0 * time) as u32)
    }
}

#[derive(Debug, PartialEq)]
pub struct Smf<'a>(pub midly::Smf<'a>);

impl<'a> Deref for Smf<'a> {
    type Target = midly::Smf<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> Smf<'a> {
    pub fn new(header: Header) -> Smf<'a> {
        Smf(midly::Smf {
            header,
            tracks: vec![],
        })
    }
}
