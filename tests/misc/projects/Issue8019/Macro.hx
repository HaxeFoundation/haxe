import haxe.macro.Expr;
import haxe.macro.Context;

class Macro {
	static function init() {
		function defineType(pack, kind) {
			Context.defineType({
				pos: (macro 0).pos,
				pack: pack,
				name: 'Test',
				kind: kind,
				fields: []
			});
		}

		function defineClass(pack) {
			defineType(pack, TDClass());
		}

		defineClass(["Module"]);
		defineClass(["pack", "Module"]);

		defineClass([""]);
		defineClass(["\n"]);
		defineClass(["pack\n"]);
		defineClass(["pack~"]);
		defineClass(["Foo", "Bar"]);
		
		defineClass(["0_class"]);
		defineType(["0_enum"], TDEnum);
		defineType(["0_structure"], TDStructure);
		defineType(["0_abstract"], TDAbstract(TPath({pack: [], name: "Int"})));
	}
}
