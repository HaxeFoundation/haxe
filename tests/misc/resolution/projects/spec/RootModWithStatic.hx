class RootModWithStatic {
    public static function TheStatic() utest.Assert.fail();
}

@:build(Macro.build()) class TheStatic {}
