#if macro
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
using haxe.macro.Tools;
#end

class Macro {
	macro public static function f(a, b) {
		var t = Context.typeof(a);
		var ct = generateDerivedType(t);
		var r = macro @:pos(b.pos) ($b : $ct);
		return r;
	}

	#if macro
	static function generateDerivedType(t:Type):ComplexType {
		var cl = switch (t) {
			case TInst(_.get() => cl, _): cl;
			case _: throw "nope";
		};

		var derivedName = cl.name + "__Derived";
		try {
			Context.getType(derivedName);
		} catch (e) {
			var derivedFields:Array<Field> = [for (f in cl.fields.get()) {pos: f.pos, name: f.name, kind: FVar(f.type.toComplexType()), meta: [{name: ":optional", pos: f.pos}]}];
			Context.defineType({
				pos: cl.pos,
				pack: [],
				name: derivedName,
				kind: TDStructure,
				fields: derivedFields,
			}, cl.module);
		}

		return TPath({pack: [], name: derivedName});
	}
	#end
}