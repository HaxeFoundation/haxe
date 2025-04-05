package unit.issues.misc;

import haxe.macro.Context;
import haxe.macro.Type;
import haxe.macro.Expr;
using haxe.macro.Tools;

class Issue5207Macro {
	macro static public function test(){
		var ret = [];
        // build a static function with a constrained type parameter
        var td = macro class C {
            public static function func<T:{a:Int}>(v:T) return v;
        }
        Context.defineType(td);
        var t = Context.getType("C");
        var expr = macro { var r = C.func({b:""}); };
        var success = try {Context.typeExpr(expr);true;} catch (e:Dynamic) false;
        ret.push(macro f($v{success}));

        // build a generic abstract with a constrained type parameter + from T conversion
        var td2 = macro class A<T:{a:Int}> {
            public static function func<T:{a:Int}>(v:T) return v;
        }
        td2.kind = TDAbstract(macro : {},[macro : T],[]);
        Context.defineType(td2);
        var t2 = Context.getType("A");
        var expr2 = macro {b:""};
        var etype = Context.typeof(expr2);
        var success = Context.unify(etype,t2);
        ret.push(macro f($v{success}));
        return macro $b{ret};
	}
}