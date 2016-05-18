class Macro {
    static function buildSomething() {
        haxe.macro.Context.defineType(macro class K { });
        return null;
    }
}