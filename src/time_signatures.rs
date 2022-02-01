use crate::grammar::{
    abc_time_signature,
    TimeSignatureSymbol::{AllaBreve, CommonTime, Meter},
};
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

pub fn parse_abc_time_signature_to_midi(info_field_m: &str) -> Result<MetaMessage> {
    let (numerator, denominator) = match abc_time_signature::time_signature(info_field_m)? {
        Some(AllaBreve) => (2, 2),
        Some(CommonTime) => (4, 4),
        Some(Meter(num, denom)) => (num, denom),
        None => (4, 4),
    };
    let midi_clocks_per_click = match numerator {
        3 => 18, // TODO: why?
        _ => 48,
    };
    let demisemiquavers_per_crotchet = 8;
    let meter = TimeSignature(
        numerator,
        denominator / 2, // TODO: why?
        midi_clocks_per_click,
        demisemiquavers_per_crotchet,
    );
    Ok(meter)
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

    #[rstest(
        info_field_m,
        expect,
        case::common_time("C", TimeSignature(4, 2, 48, 8)),
        case::alla_breve("C|", TimeSignature(2, 1, 48, 8)),
        case::four_four("4/4", TimeSignature(4, 2, 48, 8)),
        case::three_four("3/4", TimeSignature(3, 2, 18, 8)),
        case::two_two("2/2", TimeSignature(2, 1, 48, 8)),
        case::empty("", TimeSignature(4, 2, 48, 8))
    )]
    fn test_parse_abc_time_signature_to_midi(info_field_m: &str, expect: MetaMessage) {
        let result = parse_abc_time_signature_to_midi(info_field_m).unwrap();
        assert_eq!(result, expect);
    }
}
