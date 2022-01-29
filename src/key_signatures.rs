use crate::{
    abc_wrappers::{DiatonicPitchClass, MaybeAccidental},
    accidentals::KeySignatureMap,
    errors::{PitchConversionError, Result},
};
use abc_parser::datatypes::{
    Accidental::{DoubleFlat, DoubleSharp, Flat, Sharp},
    Note::{A, B, C, D, E, F, G},
};
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
        Err(Box::new(PitchConversionError))
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
    pub fn from_note_and_accidental(note: DiatonicPitchClass, accidental: MaybeAccidental) -> Self {
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
            MaybeAccidental(Some(DoubleSharp)) => 2,
            MaybeAccidental(Some(Sharp)) => 1,
            MaybeAccidental(Some(Flat)) => -1,
            MaybeAccidental(Some(DoubleFlat)) => -2,
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
pub fn key_signature_for_major(root_key: PitchClass, flat: bool) -> MetaMessage<'static> {
    if root_key.is_natural() && flat {
        panic!("{root_key:#?} is a white key, can't make it flat")
    }
    let signature = ((root_key.on_fifth_circle() + 1) % 12) as i8 - 1;
    let minor = false;
    midly::MetaMessage::KeySignature(
        if flat && signature > 5 {
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
    use rstest::rstest;

    const SIX_FLATS: MetaMessage = MetaMessage::KeySignature(-6, false);
    const FIVE_FLATS: MetaMessage = MetaMessage::KeySignature(-5, false);
    const FOUR_FLATS: MetaMessage = MetaMessage::KeySignature(-4, false);
    const THREE_FLATS: MetaMessage = MetaMessage::KeySignature(-3, false);
    const TWO_FLATS: MetaMessage = MetaMessage::KeySignature(-2, false);
    const ONE_FLAT: MetaMessage = MetaMessage::KeySignature(-1, false);
    const ZERO_SHARPS: MetaMessage = MetaMessage::KeySignature(0, false);
    const ONE_SHARP: MetaMessage = MetaMessage::KeySignature(1, false);
    const TWO_SHARPS: MetaMessage = MetaMessage::KeySignature(2, false);
    const THREE_SHARPS: MetaMessage = MetaMessage::KeySignature(3, false);
    const FOUR_SHARPS: MetaMessage = MetaMessage::KeySignature(4, false);
    const FIVE_SHARPS: MetaMessage = MetaMessage::KeySignature(5, false);
    const SIX_SHARPS: MetaMessage = MetaMessage::KeySignature(6, false);
    const SEVEN_SHARPS: MetaMessage = MetaMessage::KeySignature(7, false);
    const EIGHT_SHARPS: MetaMessage = MetaMessage::KeySignature(8, false);
    const NINE_SHARPS: MetaMessage = MetaMessage::KeySignature(9, false);
    const TEN_SHARPS: MetaMessage = MetaMessage::KeySignature(10, false);

    impl PitchClass {
        // Pitches only used in tests
        const DF: Self = Self(1);
        const EF: Self = Self(3);
        const FS: Self = Self(6);
        const AF: Self = Self(8);
        const BF: Self = Self(10);
    }
    #[rstest(
        root, flat, expect,
        case::c(0, false, ZERO_SHARPS),
        case::cs(1, false, SEVEN_SHARPS),
        case::df(1, true, FIVE_FLATS),
        case::d(2, false, TWO_SHARPS),
        case::ds(3, false, NINE_SHARPS),
        case::ef(3, true, THREE_FLATS),
        case::e(4, false, FOUR_SHARPS),
        case::f(5, false, ONE_FLAT),
        case::fs(6, false, SIX_SHARPS),
        case::gf(6, true, SIX_FLATS),
        case::g(7, false, ONE_SHARP),
        case::gs(8, false, EIGHT_SHARPS),
        case::af(8, true, FOUR_FLATS),
        case::a(9, false, THREE_SHARPS),
        case::as_(10, false, TEN_SHARPS),
        case::bf(10, true, TWO_FLATS),
        case::b(11, false, FIVE_SHARPS),
        case::c2(12, false, ZERO_SHARPS),
        case::df3(25, true, FIVE_FLATS),
        case::d4(38, false, TWO_SHARPS),
        case::ef5(51, true, THREE_FLATS),
        case::e6(64, false, FOUR_SHARPS),
        case::f7(77, false, ONE_FLAT),
        case::fs8(90, false, SIX_SHARPS),
        case::g9(103, false, ONE_SHARP),
        case::af10(116, true, FOUR_FLATS)
THREE_SHARPS)]
    fn key_signature(root: u16, flat: bool, expect: MetaMessage) {
        let result = key_signature_for_major(PitchClass::from(root), flat);
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
}
