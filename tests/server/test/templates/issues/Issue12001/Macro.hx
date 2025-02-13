import haxe.macro.Context;
import haxe.macro.Expr.Error;

@:persistent var i = 0;
function defineType() {
	Context.onAfterInitMacros(() -> {
		Context.defineType({
			pos: Context.currentPos(),
			pack: [],
			name: "Foo",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Foo {
				public static function test() Sys.println("Foo.test() = " + $v{i++});
			}).fields
		});
	});
}

@:persistent var j = 0;
function defineModule() {
	Context.onAfterInitMacros(() -> {
		Context.defineModule("Bar", [{
			pos: Context.currentPos(),
			pack: [],
			name: "Bar",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Bar {
				public static function test() Sys.println("Bar.test() = " + $v{j++});
			}).fields
		}]);
	});
}

@:persistent var k = 0;
function hookRedefine() {
	var generated = false;
	Context.onAfterTyping((_) -> {
		if (generated) return;
		generated = true;

		Context.defineModule("Baz", [{
			pos: Context.currentPos(),
			pack: [],
			name: "Baz",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Baz {
				public static function __init__() Sys.println("Baz.test() = " + $v{k++});
			}).fields
		}]);
	});
}

@:persistent var l = 0;
function redefineTypeCatchError() {
	Context.onAfterInitMacros(() -> {
		if (l > 0) trace(Context.getType("Foobar"));

		try {
			l++;
			Context.defineType({
				pos: Context.currentPos(),
				pack: [],
				name: "Foobar",
				kind: TDClass(null, null, false, false, false),
				fields: []
			});
		} catch (e:Error) {
			if (l == 0) throw e;
			trace(e.message);
		}
	});
}

@:persistent var m = 0;
function redefineModuleCatchError() {
	Context.onAfterInitMacros(() -> {
		if (m > 0) trace(Context.getType("Foobaz"));

		try {
			m++;
			Context.defineModule("Foobaz", [{
				pos: Context.currentPos(),
				pack: [],
				name: "Foobaz",
				kind: TDClass(null, null, false, false, false),
				fields: []
			}]);
		} catch (e:Error) {
			if (m == 0) throw e;
			trace(e.message);
		}
	});
}
