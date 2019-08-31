import haxe.macro.Context;

class Main3 {
	static function init() {
		function defineType(name, kind) {
			Context.defineType({
				pack: [],
				name: name,
				pos: (macro 0).pos,
				kind: kind,
				fields: []
			});
		}

		defineType("lowercase", TDClass());

		defineType("0_class", TDClass());
		defineType("0_enum", TDEnum);
		defineType("0_struct", TDStructure);
		defineType("0_abstract", TDAbstract(TPath({pack: [], name: "Int"})));
	}
}
