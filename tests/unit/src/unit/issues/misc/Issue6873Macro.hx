package unit.issues.misc;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.ComplexTypeTools;
import haxe.macro.TypeTools;

class Issue6873Macro {
    macro public static function build():Array<Field> {
        var fields = Context.getBuildFields();

        var m = ComplexTypeTools.toType(macro : Map<String, Int>);
        var last = "";
		var s = "";
        do {
            s += m;
            last = Std.string(m);
            m = TypeTools.followWithAbstracts(m, false);
        } while (last != Std.string(m));

		fields.push((macro class X {
			static public var result = $v{s};
		}).fields[0]);
        return fields;
    }
}