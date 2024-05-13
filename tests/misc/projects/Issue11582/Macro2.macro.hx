import haxe.macro.Context;
import haxe.macro.Expr;

using StringTools;

class Macro2 {
	static var id = 0;

	static function registerBuild(i:String, fields:Array<Field>) {
		if (Context.getLocalClass().get().isInterface) return null;

		var hasMacros = false;
		for (f in fields) {
			if (f.name == "macros") {
				hasMacros = true;
				break;
			}
		}

		if (!hasMacros)
			fields = (macro class A {
				public static var macros = [];
			}).fields.concat(fields);

		var id = '_' + id++;
		fields.push((macro class A {
			 static var $id = {macros.push($v{i}); 0;};
		}).fields[0]);

		return fields;
	}

	static function isAsync(f:Field):Bool {
		return Lambda.exists(f.meta, m -> m.name == ":async");
	}

	@:buildOrder(Late)
	public static function buildTest() {
		var fields = haxe.macro.Context.getBuildFields();
		var asyncArg = {name: "async", type: macro :Async};

		// Add `async` arg to tests with `@:async` metadata
		for (f in fields) {
			if (!f.name.startsWith("test")) continue;

			switch f.kind {
				case FFun({args: [], ret: ret, expr: expr, params: []}) if (isAsync(f)):
					f.kind = FFun({args: [asyncArg], ret: ret, expr: expr, params: []});

				case _:
			}
		}

		return registerBuild("Base Test", fields);
	}

	public static function autoAsync() {
		var fields = haxe.macro.Context.getBuildFields();

		// Add `@:async` to all tests
		for (f in fields) {
			if (!f.name.startsWith("test")) continue;

			switch f.kind {
				case FFun(_): f.meta.push({name: ":async", params: [], pos: f.pos});
				case _:
			}
		}

		return registerBuild("Auto async", fields);
	}
}
