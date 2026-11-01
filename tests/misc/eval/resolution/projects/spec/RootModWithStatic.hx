class RootModWithStatic {
    public static function TheStatic() return "RootModWithStatic.TheStatic function";
}

@:build(Macro.build()) class TheStatic {}
