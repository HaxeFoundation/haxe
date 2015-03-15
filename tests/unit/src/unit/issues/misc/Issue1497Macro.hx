package unit.issues.misc;

class Issue1497Macro {
    macro public static function run() {
        var cl = macro class Issue1497DefinedClass {
            macro public static function test() {
                return macro 1;
            }
        };
        haxe.macro.Context.defineType(cl);
        return macro null;
    }
}