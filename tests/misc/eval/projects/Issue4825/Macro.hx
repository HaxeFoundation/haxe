class Macro {
    static function buildSomething():haxe.macro.Expr.ComplexType {
        haxe.macro.Context.defineType(macro class K { });
        return null;
    }
}