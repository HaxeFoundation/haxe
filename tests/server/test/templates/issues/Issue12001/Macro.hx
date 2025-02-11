import haxe.macro.Context;

function defineType() {
	Context.onAfterInitMacros(() -> {
		Context.defineType({
			pos: Context.currentPos(),
			pack: [],
			name: "Foo",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Foo {
				public static function test() {}
			}).fields
		});
	});
}

function defineModule() {
	Context.onAfterInitMacros(() -> {
		Context.defineModule("Bar", [{
			pos: Context.currentPos(),
			pack: [],
			name: "Bar",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Bar {
				public static function test() {}
			}).fields
		}]);
	});
}

function hook() {
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
				public static function test() {}
			}).fields
		}]);
	});

	return null;
}
