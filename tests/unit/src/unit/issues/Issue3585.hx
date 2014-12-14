package unit.issues;
import unit.Test;

class Issue3585 extends Test {
    function test() {
        var v = inlineUnwrap();
        eq(getType(v), "Int");
    }

    static macro function getType(v:haxe.macro.Expr) {
        var t = haxe.macro.Context.typeof(v);
        return macro $v{haxe.macro.TypeTools.toString(t)};
    }

    inline function inlineUnwrap():Int {
        var tmp:Null<Int> = 1;
        return tmp;
    }
}
