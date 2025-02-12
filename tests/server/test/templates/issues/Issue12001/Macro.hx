import haxe.macro.CompilationServer;
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

@:persistent var i = 0;
function redefineModule() {
	Context.onAfterInitMacros(() -> {
		CompilationServer.invalidateModule("Foobar");

		Context.defineModule("Foobar", [{
			pos: Context.currentPos(),
			pack: [],
			name: "Foobar",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Foobar {
				public static function test() Sys.println("Foobar.test() = " + $v{i++});
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
}

@:persistent var j = 0;
function hookRedefine() {
	var generated = false;
	Context.onAfterTyping((_) -> {
		if (generated) return;
		generated = true;

		CompilationServer.invalidateModule("Foobaz");

		Context.defineModule("Foobaz", [{
			pos: Context.currentPos(),
			pack: [],
			name: "Foobaz",
			kind: TDClass(null, null, false, false, false),
			fields: (macro class Foobaz {
				public static function __init__() Sys.println("Foobaz.test() = " + $v{j++});
			}).fields
		}]);
	});
}

function hookInvalidateError() {
	Context.onAfterTyping((_) -> {
		CompilationServer.invalidateModule("Empty");
	});
}

function hookInvalidateCatch() {
	Context.onAfterTyping((_) -> {
		try {
			CompilationServer.invalidateModule("Empty");
		} catch (e:Dynamic) {
			Sys.println(Std.string(e));
		}
	});
}
