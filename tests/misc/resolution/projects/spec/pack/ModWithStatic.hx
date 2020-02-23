package pack;

class ModWithStatic {
    public static function TheStatic() return "pack.ModWithStatic.TheStatic function";
}

@:build(Macro.build())
class TheStatic {}
