package unit.issues;

private abstract IntKey(String) {
    function new(s) this = s;
    @:to function toInt():Int return Std.parseInt(this);
    @:from static function fromInt(v:Int):IntKey return new IntKey(Std.string(v));
}


class Issue3346 extends Test {
	function test(){
		t(m());
	}

    static macro function m() {
        var actualType = haxe.macro.Context.typeof(macro 1);
        var expectedType = haxe.macro.Context.typeof(macro (null : IntKey));
        var unify = haxe.macro.Context.unify(actualType, expectedType);
        return macro $v{unify};
    }
}