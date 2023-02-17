use locspan::Meta;
use std::path::Path;
use turtle_syntax::Parse;

fn parse<P: AsRef<Path>>(path: P) {
	stderrlog::new().init().ok();
	match std::fs::read_to_string(&path) {
		Ok(buffer) => {
			match turtle_syntax::Document::parse_str(&buffer, |span| span) {
				Ok(_) => (), // success!
				Err(Meta(e, _)) => {
					log::error!("parse error: {}", e);
					panic!("parse error: {:?}", e)
				}
			}
		}
		Err(e) => {
			log::error!("unable to read file `{}`: {}", path.as_ref().display(), e);
			panic!("IO error: {:?}", e)
		}
	}
}

#[test]
fn p01() {
	parse("tests/positive/01.ttl")
}

#[test]
fn p02() {
	parse("tests/positive/02.ttl")
}

#[test]
fn p03() {
	parse("tests/positive/03.ttl")
}

#[test]
fn p04() {
	parse("tests/positive/04.ttl")
}

#[test]
fn p05() {
	parse("tests/positive/05.ttl")
}

#[test]
fn p06() {
	parse("tests/positive/06.ttl")
}

#[test]
fn p07() {
	parse("tests/positive/07.ttl")
}

#[test]
fn p08() {
	parse("tests/positive/08.ttl")
}

#[test]
fn p09() {
	parse("tests/positive/09.ttl")
}

#[test]
fn p10() {
	parse("tests/positive/10.ttl")
}

#[test]
fn p11() {
	parse("tests/positive/11.ttl")
}

#[test]
fn p12() {
	parse("tests/positive/12.ttl")
}

#[test]
fn p13() {
	parse("tests/positive/13.ttl")
}

#[test]
fn p14() {
	parse("tests/positive/14.ttl")
}

#[test]
fn p15() {
	parse("tests/positive/15.ttl")
}

#[test]
fn p16() {
	parse("tests/positive/16.ttl")
}

#[test]
fn p17() {
	parse("tests/positive/17.ttl")
}

#[test]
fn p18() {
	parse("tests/positive/18.ttl")
}

#[test]
fn p19() {
	parse("tests/positive/19.ttl")
}

#[test]
fn p20() {
	parse("tests/positive/20.ttl")
}

#[test]
fn p21() {
	parse("tests/positive/21.ttl")
}

#[test]
fn p22() {
	parse("tests/positive/22.ttl")
}

#[test]
fn p23() {
	parse("tests/positive/23.ttl")
}

#[test]
fn p24() {
	parse("tests/positive/24.ttl")
}

#[test]
fn p25() {
	parse("tests/positive/25.ttl")
}

#[test]
fn p26() {
	parse("tests/positive/26.ttl")
}

#[test]
fn p27() {
	parse("tests/positive/27.ttl")
}
