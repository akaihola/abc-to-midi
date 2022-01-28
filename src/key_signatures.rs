use std::cmp::Ordering::{Greater, Less};
use std::ops::Mul;

#[derive(Debug, PartialEq)]
#[repr(i8)]
enum Enharmony {
    Flat,
    Natural,
    Sharp,
}

#[derive(Debug, PartialEq)]
struct KeySignature(i8);

impl KeySignature {
    fn accidentals(self) -> [i8; 7] {
        let mut result = [0; 7];
        match self.0.cmp(&0) {
            Greater => {
                for i in 0..self.0 as usize {
                    result[(3 + 4 * i) % 7] = 1;
                }
            }
            Less => {
                for i in 0..-self.0 as usize {
                    result[(6 + 3 * i) % 7] = -1;
                }
            }
            _ => (),
        }
        result
    }
}

const SIX_FLATS: KeySignature = KeySignature(-6);
const FIVE_FLATS: KeySignature = KeySignature(-5);
const FOUR_FLATS: KeySignature = KeySignature(-4);
const THREE_FLATS: KeySignature = KeySignature(-3);
const TWO_FLATS: KeySignature = KeySignature(-2);
const ONE_FLAT: KeySignature = KeySignature(-1);
const ZERO_FLATS: KeySignature = KeySignature(0);
const ZERO_SHARPS: KeySignature = KeySignature(0);
const ONE_SHARP: KeySignature = KeySignature(1);
const TWO_SHARPS: KeySignature = KeySignature(2);
const THREE_SHARPS: KeySignature = KeySignature(3);
const FOUR_SHARPS: KeySignature = KeySignature(4);
const FIVE_SHARPS: KeySignature = KeySignature(5);
const SIX_SHARPS: KeySignature = KeySignature(6);
const SEVEN_SHARPS: KeySignature = KeySignature(7);
const EIGHT_SHARPS: KeySignature = KeySignature(8);
const NINE_SHARPS: KeySignature = KeySignature(9);
const TEN_SHARPS: KeySignature = KeySignature(10);

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

const FOURTH: Interval = Interval(5);
const FIFTH: Interval = Interval(7);
const OCTAVE: Interval = Interval(1);

#[derive(Clone, Copy, Debug)]
struct PitchClass(u8);

impl From<PitchClass> for u8 {
    fn from(pc: PitchClass) -> Self {
        pc.0
    }
}

impl From<u16> for PitchClass {
    fn from(n: u16) -> Self {
        Self((n % 12) as u8)
    }
}

impl<T> Mul<T> for PitchClass
where
    T: Into<u16>,
{
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        Self::from(self.0 as u16 * rhs.into())
    }
}

impl PitchClass {
    fn is_natural(self) -> bool {
        matches!(self, Self(0 | 2 | 4 | 5 | 7 | 9 | 11))
    }

    fn on_fifth_circle(self) -> u8 {
        (self.0 * u8::from(FIFTH)) % 12
    }

    const C: Self = Self(0);
    const CS: Self = Self(1);
    const DF: Self = Self(1);
    const D: Self = Self(2);
    const DS: Self = Self(3);
    const EF: Self = Self(3);
    const E: Self = Self(4);
    const F: Self = Self(5);
    const FS: Self = Self(6);
    const GF: Self = Self(6);
    const G: Self = Self(7);
    const GS: Self = Self(8);
    const AF: Self = Self(8);
    const A: Self = Self(9);
    const AS: Self = Self(10);
    const BF: Self = Self(10);
    const B: Self = Self(11);
}

struct Pitch(i16);

fn key_signature_for_major(root_key: PitchClass, flat: bool) -> KeySignature {
    if root_key.is_natural() && flat {
        panic!("{root_key:#?} is a white key, can't make it flat")
    }
    let signature = ((root_key.on_fifth_circle() + 1) % 12) as i8 - 1;
    KeySignature(if flat && signature > 5 {
        // more sharps than five, turn sharp black key major into flat
        signature - 12
    } else {
        signature
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

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
    fn key_signature(root: u16, flat: bool, expect: KeySignature) {
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
    fn key_signature_accidentals(signature: i8, expect: [i8; 7]) {
        let result = KeySignature(signature).accidentals();
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
