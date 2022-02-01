use anyhow::{bail, Result};
use midly::{
    num::u7,
    MetaMessage::{self, TimeSignature},
};

#[derive(Debug, PartialEq)]
pub struct TimeSignatureTracker(u8, u8);

// Simplified time signature tracking. Doesn't yet implement the full spec
// as described in the ABC standard v2.1.
// In particular, doesn't support changing the time signature in the middle.
// https://abcnotation.com/wiki/abc:standard:v2.1#accidental_directives
impl TimeSignatureTracker {
    pub fn new(time_signature: &MetaMessage) -> Result<Self> {
        if let TimeSignature(numerator, denominator, ..) = time_signature {
            Ok(Self(*numerator, *denominator))
        } else {
            bail!("Expected a MIDI time signature instead of {time_signature:?}");
        }
    }

    /// Applies currently active time signature as velocity to a note, chord or grace notes
    pub fn apply(&self, time: u32) -> u7 {
        let numerator = self.0;
        match (numerator, time) {
            (_, 0) => 105.into(),
            (4, 960) => 95.into(),
            _ => 80.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use midly::MetaMessage::TimeSignature;
    use rstest::rstest;

    #[rstest(
        numerator, denominator, time, expect,
        case::common_time_bar_start(4, 4, 0, 105),
        case::common_time_half_bar(4, 4, 960, 95),
        case::common_time_second_beat(4, 4, 480, 80),
        case::common_time_fourth_beat(4, 4, 960 + 480, 80),
    )]
    fn test_apply(numerator: u8, denominator: u8, time: u32, expect: u8) {
        let timesig = TimeSignature(numerator, denominator, 48, 8);
        let tracker = TimeSignatureTracker::new(&timesig).unwrap();
        let result = tracker.apply(time);
        assert_eq!(result, expect);
    }
}
