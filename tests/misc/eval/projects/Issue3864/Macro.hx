import haxe.macro.Context;
import haxe.macro.Expr;

using haxe.macro.Tools;

class Macro {
	static var counter = 0;

	static public function apply() {
		var local = Context.getLocalType();
		var expected = Context.getExpectedType();
		var superClass = switch (expected) {
			case TInst(c, [t]):
				var c = c.get();
				{ pack: c.pack, name: c.name, params: [TPType(t.toComplexType())] };
			case _:
				throw false;
		}
		var name = 'Test${counter++}';
		var cls = macro class $name extends $superClass {
			public function new() {
				super($v{name} + " extends " + $v{expected.toString()});
			}
		}
		Context.defineType(cls);
		return TPath({pack: [], name: name});
	}
}