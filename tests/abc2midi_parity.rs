use abc_parser::abc;
use abc_to_midi::midly_wrappers::Smf;
use pretty_assertions::assert_eq;


#[cfg(test)]

#[test]
fn compare() {
    let abc2midi_raw = std::fs::read("test-asset/three-quarters.mid").unwrap();
    let abc2midi_output = midly::Smf::parse(&abc2midi_raw).unwrap();

    let abc_example = std::fs::read_to_string("test-asset/three-quarters.abc").unwrap();
    let abc_parsed = abc::tune(&abc_example).unwrap();
    let Smf(abc_to_midi_output) = abc_parsed.try_into().unwrap();

    assert_eq!(abc_to_midi_output, abc2midi_output);
}