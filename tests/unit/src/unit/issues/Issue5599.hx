package unit.issues;

private abstract I(Int) from Int to Int {
    @:op(A==B)
    @:commutative
    public function eqString(s:String):Bool
        return Std.string(this) == s;
}

class Issue5599 extends unit.Test {
	function test() {
        var i:I = 1;
        t(i == '1');
        t('1' == i);
	}
}