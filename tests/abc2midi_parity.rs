#[cfg(test)]
use abc_parser::abc;
use abc_to_midi::midly_wrappers::Smf;
use pretty_assertions::assert_eq;
use rstest::rstest;

#[rstest(
    name,
    case::three_quarters("three-quarters"),
    case::chord("chord"),
    case::two_bars("two-bars"),
    case::accidentals("accidentals"),
    case::untitled_reel("untitled-reel"),
)]
fn compare(name: &str) {
    let abc2midi_raw = std::fs::read(format!("test-asset/{}.mid", name)).unwrap();
    let abc2midi_output = midly::Smf::parse(&abc2midi_raw).unwrap();

    let abc_example = std::fs::read_to_string(format!("test-asset/{}.abc", name)).unwrap();
    let abc_parsed = &abc::tune(&abc_example).unwrap();
    let Smf(abc_to_midi_output) = abc_parsed.try_into().unwrap();

    assert_eq!(abc_to_midi_output, abc2midi_output);
}