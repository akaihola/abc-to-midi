use abc_parser::datatypes::{
    Accidental::{self, Flat, Sharp},
    Note,
};

#[derive(Debug, PartialEq)]
pub struct KeySignatureSymbol {
    pub note: Note,
    pub accidental: Option<Accidental>,
    pub minor: bool,
}

peg::parser! {
/// Adds key signature parsing which is missing from abc-parser@0.3.0
pub grammar abc_key_signature() for str {
    rule note_uppercase() -> Note
        = "A" { Note::A } /
          "B" { Note::B } /
          "C" { Note::C } /
          "D" { Note::D } /
          "E" { Note::E } /
          "F" { Note::F } /
          "G" { Note::G }
    rule accidental() -> Accidental
        = "#" { Sharp } /
          "b" { Flat }
    rule minor() -> bool
        = "min" { true }
    pub rule key_signature() -> KeySignatureSymbol
        = note:note_uppercase() accidental:accidental()? m:minor()? {
            let minor = matches!(m, Some(true));
            KeySignatureSymbol { note, accidental, minor }
        }
}
}

#[derive(Debug, PartialEq)]
pub enum TimeSignatureSymbol {
    Meter(u8, u8),
    AllaBreve,
    CommonTime,
}

peg::parser! {
/// Adds key signature parsing which is missing from abc-parser@0.3.0
pub grammar abc_time_signature() for str {
    rule number() -> u8
        = n:$(['1'..='9']['0'..='9']*) { n.parse().unwrap() }
    rule meter() -> (u8, u8)
        = numerator:number() "/" denominator:number() { (numerator, denominator) }
    pub rule time_signature() -> Option<TimeSignatureSymbol>
        = "C|" { Some(TimeSignatureSymbol::AllaBreve) } /
          "C" { Some(TimeSignatureSymbol::CommonTime) } /
          m:meter() { Some(TimeSignatureSymbol::Meter(m.0, m.1)) } /
          "" { None }
}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::abc_key_signature;
    use abc_parser::datatypes::Note::{A, B, C, D, E, F, G};
    use rstest::rstest;

    impl KeySignatureSymbol {
        fn new(note: Note, accidental: Option<Accidental>, minor: bool) -> Self {
            Self {
                note,
                accidental,
                minor,
            }
        }
    }

    #[rstest(
        info_field_k,
        expect_note,
        expect_accidental,
        expect_minor,
        case::cb("Cb", C, Some(Flat), false),
        case::cbm("Cbmin", C, Some(Flat), true),
        case::c("C", C, None, false),
        case::cm("Cmin", C, None, true),
        case::cs("C#", C, Some(Sharp), false),
        case::csm("C#min", C, Some(Sharp), true),
        case::db("Db", D, Some(Flat), false),
        case::dbm("Dbmin", D, Some(Flat), true),
        case::d("D", D, None, false),
        case::dm("Dmin", D, None, true),
        case::ds("D#", D, Some(Sharp), false),
        case::dsm("D#min", D, Some(Sharp), true),
        case::eb("Eb", E, Some(Flat), false),
        case::ebm("Ebmin", E, Some(Flat), true),
        case::e("E", E, None, false),
        case::em("Emin", E, None, true),
        case::es("E#", E, Some(Sharp), false),
        case::esm("E#min", E, Some(Sharp), true),
        case::fb("Fb", F, Some(Flat), false),
        case::fbm("Fbmin", F, Some(Flat), true),
        case::f("F", F, None, false),
        case::fm("Fmin", F, None, true),
        case::fs("F#", F, Some(Sharp), false),
        case::fsm("F#min", F, Some(Sharp), true),
        case::gb("Gb", G, Some(Flat), false),
        case::gbm("Gbmin", G, Some(Flat), true),
        case::g("G", G, None, false),
        case::gm("Gmin", G, None, true),
        case::gs("G#", G, Some(Sharp), false),
        case::gsm("G#min", G, Some(Sharp), true),
        case::ab("Ab", A, Some(Flat), false),
        case::abm("Abmin", A, Some(Flat), true),
        case::a("A", A, None, false),
        case::am("Amin", A, None, true),
        case::as_("A#", A, Some(Sharp), false),
        case::asm("A#min", A, Some(Sharp), true),
        case::bb("Bb", B, Some(Flat), false),
        case::bbm("Bbmin", B, Some(Flat), true),
        case::b("B", B, None, false),
        case::bm("Bmin", B, None, true),
        case::bs_("B#", B, Some(Sharp), false),
        case::bsm("B#min", B, Some(Sharp), true)
    )]
    fn test_parse_abc_key_signature_to_midi(
        info_field_k: &str,
        expect_note: Note,
        expect_accidental: Option<Accidental>,
        expect_minor: bool,
    ) {
        let result = abc_key_signature::key_signature(info_field_k).unwrap();
        let expect = KeySignatureSymbol::new(expect_note, expect_accidental, expect_minor);
        assert_eq!(result, expect);
    }
}
