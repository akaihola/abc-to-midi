use crate::{
    abc_wrappers::{DiatonicPitchClass},
    accidentals::KeySignatureMap,
    errors::AbcParseError,
};
use abc_parser::datatypes::{
    Accidental::{self, DoubleFlat, DoubleSharp, Flat, Sharp},
    Note::{A, B, C, D, E, F, G},
};
use anyhow::Result;
use derive_more::Into;
use midly::MetaMessage;
use num_traits::PrimInt;
use std::{
    cmp::Ordering::{Greater, Less},
    ops::{Add, Mul},
};

pub type KeySignatureTable = [i8; 7];

fn accidentals_table(meta_message: MetaMessage) -> Result<KeySignatureTable> {
    if let MetaMessage::KeySignature(num_signs, _minor) = meta_message {
        let mut result = [0; 7];
        match num_signs.cmp(&0) {
            Greater => {
                for i in 0..num_signs as usize {
                    result[(3 + 4 * i) % 7] = 1;
                }
            }
            Less => {
                for i in 0..-num_signs as usize {
                    result[(6 + 3 * i) % 7] = -1;
                }
            }
            _ => (),
        }
        Ok(result)
    } else {
        Err(
            AbcParseError::WrongMidiMetaMessageKind(format!("{meta_message:#?}"), "KeySignature")
                .into(),
        )
    }
}

pub fn key_signature(meta_message: MetaMessage) -> Result<KeySignatureMap> {
    let mut result = KeySignatureMap::new();
    for (diatonic_pitch, accidental) in accidentals_table(meta_message)?.iter().enumerate() {
        // table size is always 7, safe to unwrap conversion to a diatonic pitch class
        let note: DiatonicPitchClass = diatonic_pitch.try_into().unwrap();
        match accidental {
            -1 => {
                result.insert(note, Flat);
            }
            1 => {
                result.insert(note, Sharp);
            }
            _ => (),
        }
    }
    Ok(result)
}

struct Interval(u8);

impl From<Interval> for u8 {
    fn from(i: Interval) -> Self {
        i.0
    }
}

impl From<Interval> for u16 {
    fn from(i: Interval) -> Self {
        i.0.into()
    }
}

const FIFTH: Interval = Interval(7);

/// Calculates the least nonnegative remainder of `lhs (mod rhs)`.
///
/// We need a local reimplementation until
/// https://github.com/rust-num/num-traits/pull/195 is merged and released
///
/// See https://doc.rust-lang.org/core/primitive.i8.html#method.rem_euclid
pub fn rem_euclid<T>(lhs: T, rhs: T) -> T
where
    T: PrimInt,
{
    let r = lhs % rhs;
    if r < T::zero() {
        if rhs < T::zero() {
            r - rhs
        } else {
            r + rhs
        }
    } else {
        r
    }
}

pub trait MyMarker {}
impl MyMarker for i8 {}
impl MyMarker for i16 {}
impl MyMarker for i32 {}
impl MyMarker for i64 {}
impl MyMarker for i128 {}
impl MyMarker for isize {}
impl MyMarker for u8 {}
impl MyMarker for u16 {}
impl MyMarker for u32 {}
impl MyMarker for u64 {}
impl MyMarker for u128 {}
impl MyMarker for usize {}

#[derive(Clone, Copy, Debug, Into)]
pub struct PitchClass(u8);

impl<T: PrimInt> From<T> for PitchClass {
    fn from(integer: T) -> Self {
        // These are safe to `unwrap()`, guaranteed to be in range 0..12
        let rhs = T::from(12u8).unwrap();
        let modulo = rem_euclid(integer, rhs).to_u8().unwrap();
        Self(modulo)
    }
}

impl<T: PrimInt> Mul<T> for PitchClass {
    type Output = Self;

    /// Multiplies a pitch class by an integer,
    /// wrapping around using modular arithmetic.
    /// Will panic if the product before modulo > 65535,
    /// so the largest safe multiplicand is floor(65535 / 11) = 5957.
    fn mul(self, rhs: T) -> Self::Output {
        let pitch_class_int = self.0 as u16;
        let multiplier = rhs.to_u16().unwrap();
        (multiplier * pitch_class_int).into()
    }
}

impl<T: PrimInt> Add<T> for PitchClass {
    type Output = Self;

    fn add(self, other: T) -> Self::Output {
        // convert right hand side to a small
        let rhs: T = T::from(12u8).unwrap();
        let other_scaled = rem_euclid(other, rhs).to_u8().unwrap();
        let n = self.0 + other_scaled;
        n.into()
    }
}

impl PitchClass {
    /// Converts a `(abc_wrappers::Note, abc_wrappers::MaybeAccidental)` tuple
    /// into a `PitchClass` struct
    pub fn from_note_and_accidental(
        note: DiatonicPitchClass,
        accidental: Option<Accidental>,
    ) -> Self {
        let natural = match note {
            DiatonicPitchClass(C) => PitchClass::C,
            DiatonicPitchClass(D) => PitchClass::D,
            DiatonicPitchClass(E) => PitchClass::E,
            DiatonicPitchClass(F) => PitchClass::F,
            DiatonicPitchClass(G) => PitchClass::G,
            DiatonicPitchClass(A) => PitchClass::A,
            DiatonicPitchClass(B) => PitchClass::B,
        };
        let accidental_adjust = match accidental {
            Some(DoubleSharp) => 2,
            Some(Sharp) => 1,
            Some(Flat) => -1,
            Some(DoubleFlat) => -2,
            _ => 0,
        };
        natural + accidental_adjust
    }

    fn is_natural(self) -> bool {
        matches!(self, Self(0 | 2 | 4 | 5 | 7 | 9 | 11))
    }

    fn on_fifth_circle(self) -> u8 {
        (self.0 * u8::from(FIFTH)) % 12
    }

    const C: Self = Self(0);
    const D: Self = Self(2);
    const E: Self = Self(4);
    const F: Self = Self(5);
    const G: Self = Self(7);
    const A: Self = Self(9);
    const B: Self = Self(11);
}

/// Returns the number of sharps (>0) or flats (<0) in the major key for the given
/// pitch class (C, C#, D, ...). For black-key majors, `flat==true` will force the
/// enharmonic flat root key to be used insted of the sharp one.
pub fn get_signature_for_diatonic_key(
    root_key: PitchClass,
    flat: bool,
    minor: bool,
) -> MetaMessage<'static> {
    if root_key.is_natural() && flat {
        panic!("{root_key:#?} is a white key, can't make it flat")
    }
    let offset: i8 = if minor { 3 } else { 0 };
    let circle = root_key.on_fifth_circle() as i8 - offset;
    let center = 1 + offset;
    let signature = rem_euclid(circle + center, 12) - center;
    midly::MetaMessage::KeySignature(
        if flat && signature > 5 {
            // TODO: adjust this for minors
            // more sharps than five, turn sharp black key major into flat
            signature - 12
        } else {
            signature
        },
        minor,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use abc_parser::datatypes::{
        Accidental::{Flat, Sharp},
        Note::{self, A, B, C, D, E, F, G},
    };
    use midly::MetaMessage::KeySignature;
    use rstest::rstest;

    impl PitchClass {
        // Pitches only used in tests
        const DF: Self = Self(1);
        const EF: Self = Self(3);
        const FS: Self = Self(6);
        const AF: Self = Self(8);
        const BF: Self = Self(10);
    }
    #[rstest(
        root, flat, minor, expect,
        case::c(0, false, false, KeySignature(0, false)),
        case::cs(1, false, false, KeySignature(7, false)),
        case::df(1, true, false, KeySignature(-5, false)),
        case::d(2, false, false, KeySignature(2, false)),
        case::ef(3, true, false, KeySignature(-3, false)),
        case::e(4, false, false, KeySignature(4, false)),
        case::f(5, false, false, KeySignature(-1, false)),
        case::fs(6, false, false, KeySignature(6, false)),
        case::gf(6, true, false, KeySignature(-6, false)),
        case::g(7, false, false, KeySignature(1, false)),
        case::af(8, true, false, KeySignature(-4, false)),
        case::a(9, false, false, KeySignature(3, false)),
        case::bf(10, true, false, KeySignature(-2, false)),
        case::b(11, false, false, KeySignature(5, false)),
        case::c2(12, false, false, KeySignature(0, false)),
        case::df3(25, true, false, KeySignature(-5, false)),
        case::d4(38, false, false, KeySignature(2, false)),
        case::ef5(51, true, false, KeySignature(-3, false)),
        case::e6(64, false, false, KeySignature(4, false)),
        case::f7(77, false, false, KeySignature(-1, false)),
        case::fs8(90, false, false, KeySignature(6, false)),
        case::g9(103, false, false, KeySignature(1, false)),
        case::af10(116, true, false, KeySignature(-4, false)),
        case::cm(0, false, true, KeySignature(-3, true)),
        case::csm(1, false, true, KeySignature(4, true)),
        case::dm(2, false, true, KeySignature(-1, true)),
        case::dsm(3, false, true, KeySignature(6, true)),
        case::efm(3, true, true, KeySignature(-6, true)),
        case::em(4, false, true, KeySignature(1, true)),
        case::fm(5, false, true, KeySignature(-4, true)),
        case::fsm(6, false, true, KeySignature(3, true)),
        case::gm(7, false, true, KeySignature(-2, true)),
        case::gsm(8, false, true, KeySignature(5, true)),
        case::am(9, false, true, KeySignature(0, true)),
        case::bfm(10, true, true, KeySignature(-5, true)),
        case::bm(11, false, true, KeySignature(2, true)),
        case::cm2(12, false, true, KeySignature(-3, true)),
        case::dm4(38, false, true, KeySignature(-1, true)),
        case::efm5(51, true, true, KeySignature(-6, true)),
        case::em6(64, false, true, KeySignature(1, true)),
        case::fm7(77, false, true, KeySignature(-4, true)),
        case::fsm8(90, false, true, KeySignature(3, true)),
        case::gm9(103, false, true, KeySignature(-2, true)),
)]
    fn test_get_signature_for_diatonic_key(
        root: u16,
        flat: bool,
        minor: bool,
        expect: MetaMessage,
    ) {
        let result = get_signature_for_diatonic_key(PitchClass::from(root), flat, minor);
        assert_eq!(result, expect);
    }

    #[rstest(
        signature, expect,
        case::c(0, [0, 0, 0, 0, 0, 0, 0]),
        case::g(1, [0, 0, 0, 1, 0, 0, 0]),
        case::d(2, [1, 0, 0, 1, 0, 0, 0]),
        case::a(3,  [1, 0, 0, 1, 1, 0, 0]),
        case::e(4,  [1, 1, 0, 1, 1, 0, 0]),
        case::b(5,  [1, 1, 0, 1, 1, 1, 0]),
        case::fs(6,  [1, 1, 1, 1, 1, 1, 0]),
        case::cs(7,  [1, 1, 1, 1, 1, 1, 1]),
        case::f(-1,  [0, 0, 0, 0, 0, 0, -1]),
        case::bf(-2,  [0, 0, -1, 0, 0, 0, -1]),
        case::ef(-3,  [0, 0, -1, 0, 0, -1, -1]),
        case::af(-4,  [0, -1, -1, 0, 0, -1, -1]),
        case::df(-5,  [0, -1, -1, 0, -1, -1, -1]),
        case::gf(-6,  [-1, -1, -1, 0, -1, -1, -1]),
        case::cf(-7,  [-1, -1, -1, -1, -1, -1, -1]),
    )]
    fn key_signature_accidentals(signature: i8, expect: KeySignatureTable) {
        let key_signature = MetaMessage::KeySignature(signature, false);
        let result = accidentals_table(key_signature).unwrap();
        assert_eq!(result, expect);
    }

    #[rstest(
        pitch_class,
        expect,
        case(0, true),
        case(1, false),
        case(2, true),
        case(3, false),
        case(4, true),
        case(5, true),
        case(6, false),
        case(7, true),
        case(8, false),
        case(9, true),
        case(10, false),
        case(11, true)
    )]
    fn pitch_class_is_natural(pitch_class: u16, expect: bool) {
        let result = PitchClass::from(pitch_class).is_natural();
        assert_eq!(result, expect);
    }

    #[rstest(
        pitch_class,
        expect,
        case(PitchClass::C, 0),
        case(PitchClass::G, 1),
        case(PitchClass::D, 2),
        case(PitchClass::A, 3),
        case(PitchClass::E, 4),
        case(PitchClass::B, 5),
        case(PitchClass::FS, 6),
        case(PitchClass::DF, 7),
        case(PitchClass::AF, 8),
        case(PitchClass::EF, 9),
        case(PitchClass::BF, 10),
        case(PitchClass::F, 11)
    )]
    fn pitch_class_on_fifth_circle(pitch_class: PitchClass, expect: u8) {
        let result = pitch_class.on_fifth_circle();
        assert_eq!(result, expect);
    }

    #[rstest(
        note,
        accidental,
        expect,
        case::cf(C, Some(Flat), 11),
        case::c(C, None, 0),
        case::c(C, Some(Sharp), 1),
        case::df(D, Some(Flat), 1),
        case::d(D, None, 2),
        case::d(D, Some(Sharp), 3),
        case::ef(E, Some(Flat), 3),
        case::e(E, None, 4),
        case::e(E, Some(Sharp), 5),
        case::ff(F, Some(Flat), 4),
        case::f(F, None, 5),
        case::f(F, Some(Sharp), 6),
        case::gf(G, Some(Flat), 6),
        case::g(G, None, 7),
        case::g(G, Some(Sharp), 8),
        case::af(A, Some(Flat), 8),
        case::a(A, None, 9),
        case::a(A, Some(Sharp), 10),
        case::bf(B, Some(Flat), 10),
        case::b(B, None, 11),
        case::b(B, Some(Sharp), 0)
    )]
    fn test_pitch_class_from_note_and_accidental(
        note: Note,
        accidental: Option<Accidental>,
        expect: u8,
    ) {
        let diatonic_pitch_class = DiatonicPitchClass(note);
        let PitchClass(root) =
            PitchClass::from_note_and_accidental(diatonic_pitch_class, accidental);
        assert_eq!(root, expect);
    }
}
