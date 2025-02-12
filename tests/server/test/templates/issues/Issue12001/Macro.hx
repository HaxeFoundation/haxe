import haxe.macro.Context;

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
