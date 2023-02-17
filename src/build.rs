use std::collections::HashMap;
use iref::IriBuf;
use locspan::Loc;

/// Located quad.
pub type LocQuad<F> = Loc<rdf_types::Quad<
	Loc<rdf_types::Subject, F>,
	Loc<IriBuf, F>,
	Loc<rdf_types::loc::Object<F>, F>,
	Loc<rdf_types::GraphLabel, F>
>, F>;

pub struct Context<F> {
	prefixes: HashMap<String, Loc<IriBuf, F>>
}

pub trait Build<F> {
	type Target;

	fn build(self, ctx: &mut Context<F>) -> Self::Target;
}

impl<F> Build<F> for crate::Document<F> {
	type Target = Vec<LocQuad<F>>;

	fn build(self, ctx: &mut Context<F>) -> Self::Target {
		let mut quads = Vec::new();

		for statement in self.quads {
			// ...
		}

		quads
	}
}