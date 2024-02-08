package unit.issues;

class Issue10209 extends Test {
	function test() {
		var s:AStr = 'hello';
		s += ', world';
		eq('hello, world', s);
	}
}

private abstract AStr(String) from String to String {
	@:op(A += B) @:commutative static function assignAdd(a:AStr, b:String):AStr;
}