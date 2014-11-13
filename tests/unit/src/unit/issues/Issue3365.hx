package unit.issues;

private abstract IntKey(String) {
    @:from inline static function fromInt(v:Int):IntKey return cast "";
}

class Issue3365 extends Test {

	function test() {
		t(func());
	}

    static macro function func() {
        var t1 = haxe.macro.Context.typeof(macro 1);
        var t2 = haxe.macro.Context.typeof(macro (null : IntKey));
        var r = haxe.macro.Context.unify(t1, t2);
		return macro $v{r};
    }
}