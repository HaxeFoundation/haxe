#if macro
import haxe.macro.Expr;
import haxe.macro.Context;
using haxe.macro.Tools;
#end

class Macro {
    #if macro
    static function build():Array<Field> {
        var dotPath = Context.getLocalClass().toString();
        return (macro class {
            public final f = $v{dotPath};
            public function new() {}
            public static function UpperCase() return $v{dotPath + ".UpperCase"};
            public static function lowerCase() return $v{dotPath + ".lowerCase"};
        }).fields;
    }
    #end

    public static macro function assert(path:String) {
        var pos = Context.currentPos();
        var nameExpr = Context.parse("new " + path + "().f", pos);
        var uCallExpr = Context.parse(path + ".UpperCase()", pos);
        var lCallExpr = Context.parse(path + ".lowerCase()", pos);
        var fullPath = Context.getType(path).toString();
        return macro @:pos(pos) {
            utest.Assert.equals($v{fullPath}, $nameExpr);
            utest.Assert.equals($v{fullPath + ".UpperCase"}, $uCallExpr);
            utest.Assert.equals($v{fullPath + ".lowerCase"}, $lCallExpr);
        };
    }

    public static macro function resolves(path:String) {
        return try { Context.getType(path); macro true; } catch (_:Any) macro false;
    }
}
