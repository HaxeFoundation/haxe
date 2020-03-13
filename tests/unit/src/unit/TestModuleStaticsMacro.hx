package unit;

#if !macro
@:genericBuild(unit.TestModuleStaticsMacro.build()) class TestModuleStaticsMacro<T> {}
#else
import haxe.macro.Context;

class TestModuleStaticsMacro {
	static function build() {
		var pos = Context.currentPos();

		// a bit awkward, but oh well
		Context.defineModule("mstatics.Funcs", [
			{
				pos: pos,
				name: "funcA",
				kind: TDStatic(FFun({ret: macro : Int, args: [], expr: macro return 42})),
				pack: ["mstatics"],
				fields: []
			},
			{
				pos: pos,
				name: "funcB",
				kind: TDStatic(FFun({ret: macro : Int, args: [], expr: macro return 43})),
				pack: ["mstatics"],
				fields: []
			}
		]);

		Context.defineType({
			pos: pos,
			pack: ["mstatics"],
			name: "FuncC",
			kind: TDStatic(FFun({ret: macro : Int, args: [], expr: macro return 44})),
			fields: []
		});

		return macro : Void;
	}
}
#end
