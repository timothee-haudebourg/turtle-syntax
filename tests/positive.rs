use nquads_syntax::Parse;
use rdf_types::RdfDisplay;
use turtle_syntax::Parse as ParseNQuads;

struct Test {
	input: &'static str,
	expected_output: &'static str,
}

impl Test {
	pub fn run(self) {
		let ast = turtle_syntax::Document::parse_str(
			&std::fs::read_to_string(self.input).unwrap(),
			|span| span,
		)
		.unwrap();
		let mut generator = rdf_types::generator::Blank::new();
		let mut triples: Vec<_> = ast
			.build_triples(None, &mut generator)
			.unwrap()
			.into_iter()
			.map(|t| t.into_value().strip_all_but_predicate())
			.collect();
		triples.sort();
		triples.dedup();

		let mut expected_triples: Vec<_> = nquads_syntax::Document::parse_str(
			&std::fs::read_to_string(self.expected_output).unwrap(),
			|span| span,
		)
		.unwrap()
		.into_value()
		.into_iter()
		.map(|q| q.into_value().strip_all_but_predicate().into_triple().0)
		.collect();
		expected_triples.sort();

		let eq = triples == expected_triples;

		if !eq {
			for t in &triples {
				eprintln!("{} .", t.rdf_display())
			}
		}

		assert!(eq)
	}
}

macro_rules! positive_test {
	($($id:ident),*) => {
		$(
			#[test]
			fn $id () {
				Test {
					input: concat!("tests/positive/", stringify!($id) ,".ttl"),
					expected_output: concat!("tests/positive/", stringify!($id) ,".nq"),
				}.run()
			}
		)*
	};
}

positive_test! {
	p01,
	p02,
	p03,
	p04,
	p05,
	p06,
	p07,
	p08,
	p09,
	p10,
	p11,
	p12,
	p13,
	p14,
	p15,
	p16,
	p17,
	p18,
	p19,
	p20,
	p21,
	p22,
	p23,
	p24,
	p25,
	p26,
	p27,
	p28
}
