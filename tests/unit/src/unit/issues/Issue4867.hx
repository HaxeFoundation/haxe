package unit.issues;

private enum abstract EnumAbstract(Int) to Int {
    var a = 0;
    var b = 1;

    @:from
    public static function fromString(s:String):EnumAbstract {
        if (s == "b")
            return b;
        return a;
    }
}

class Issue4867 extends Test {
	function test() {
		var b:EnumAbstract = "b";
		eq(1, b);
	}
}