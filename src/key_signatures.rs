#[derive(PartialEq)]
#[repr(i8)]
enum Enharmony {
    Flat,
    Natural,
    Sharp,
}

type KeySignature = i8;

const SIX_FLATS: KeySignature = -6;
const FIVE_FLATS: KeySignature = -5;
const FOUR_FLATS: KeySignature = -4;
const THREE_FLATS: KeySignature = -3;
const TWO_FLATS: KeySignature = -2;
const ONE_FLAT: KeySignature = -1;
const ZERO_FLATS: KeySignature = 0;
const ZERO_SHARPS: KeySignature = 0;
const ONE_SHARP: KeySignature = 1;
const TWO_SHARPS: KeySignature = 2;
const THREE_SHARPS: KeySignature = 3;
const FOUR_SHARPS: KeySignature = 4;
const FIVE_SHARPS: KeySignature = 5;
const SIX_SHARPS: KeySignature = 6;
const SEVEN_SHARPS: KeySignature = 7;
const EIGHT_SHARPS: KeySignature = 8;
const NINE_SHARPS: KeySignature = 9;
const TEN_SHARPS: KeySignature = 10;

const FOURTH = 5;
const FIFTH: u16 = 7;
const OCTAVE: u16 = 12;

use self::Enharmony::*;

fn key_signature_for_root(root: u8, enharmony: Enharmony) -> i8 {
    let signature_plus_five: i8 = ((FIFTH * root as u16 + FOURTH) % OCTAVE)
        .try_into()
        .unwrap();
    let signature = signature_plus_five - 5;
    if enharmony == Natural && (signature < -1 || signature > 5) {
        panic!("{root} is a black key, enharmony can't be natural")
    }
    if enharmony != Natural && (signature >= -1 && signature <= 5) {
        panic!("{root} is a white key, enharmony can't be flat or sharp")
    }
    if enharmony == Sharp && signature < -1 {
        // more flats than one, it's a flat black key major
        signature + OCTAVE
    } else if enharmony == Flat && signature > 5 {
        // more sharps than five, it's a sharp black key major
        signature - OCTAVE
    } else {
        signature
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    #[rstest(
        root, enharmony, expect,
        case::c(0, Natural, ZERO_SHARPS),
        case::cs(1, Sharp, SEVEN_SHARPS),
        case::df(1, Flat, FIVE_FLATS),
        case::d(2, Natural, TWO_SHARPS),
        case::ds(3, Sharp, NINE_SHARPS),
        case::ef(3, Flat, THREE_FLATS),
        case::e(4, Natural, FOUR_SHARPS),
        case::f(5, Natural, ONE_FLAT),
        case::fs(6, Sharp, SIX_SHARPS),
        case::gf(6, Flat, SIX_FLATS),
        case::g(7, Natural, ONE_SHARP),
        case::gs(8, Sharp, EIGHT_SHARPS),
        case::af(8, Flat, FOUR_FLATS),
        case::a(9, Natural, THREE_SHARPS),
        case::as_(10, Sharp, TEN_SHARPS),
        case::bf(10, Flat, TWO_FLATS),
        case::b(11, Natural, FIVE_SHARPS),
        case::c2(12, Natural, ZERO_SHARPS),
        case::df3(25, Flat, FIVE_FLATS),
        case::d4(38, Natural, TWO_SHARPS),
        case::ef5(51, Flat, THREE_FLATS),
        case::e6(64, Natural, FOUR_SHARPS),
        case::f7(77, Natural, ONE_FLAT),
        case::fs8(90, Sharp, SIX_SHARPS),
        case::g9(103, Natural, ONE_SHARP),
        case::af10(116, Flat, FOUR_FLATS)
THREE_SHARPS)]
    fn key_signature(root: u8, enharmony: Enharmony, expect: i8) {
        let result: i8 = key_signature_for_root(root, enharmony);
        assert_eq!(result, expect);
    }
}
