package unit.issues;

private typedef Asset<@:const T> = String;

class Issue2958 extends Test {
    function test() {
        eq(
           typeString((null : Asset<["test", 1]>)),
           "unit.issues._Issue2958.Asset<[\"test\", 1]>"
        );
    }

    static macro function typeString(e)
    {
        var typed = haxe.macro.Context.typeExpr(e);
        var s = haxe.macro.TypeTools.toString(typed.t);
        return macro $v{s};
    }
}