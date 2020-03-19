package unit.issues.misc;

import haxe.macro.Type;
import haxe.macro.Expr;
import haxe.macro.Context;

using haxe.macro.TypeTools;

class Issue9147Macro {
	macro public static function typeAndReplaceTypes(anon:Expr, replacement:String):Expr {
		var t = Context.typeExpr(anon).t;
		var inMapper = [];
		function mapper(t:Type):Type {
			inMapper.push(t.toString());
			return Context.getType(replacement);
		}
		var result = [];
		switch TypeTools.map(t, mapper) {
			case TAnonymous(a):
				for(f in a.get().fields) {
					result.push('${f.name}:${f.type.toString()}');
				}
			case _:
		}
		inMapper.sort(Reflect.compare);
		return macro { foundTypes:$v{inMapper}, mappedAnon:$v{'{' + result.join(',') + '}'}}
	}
}