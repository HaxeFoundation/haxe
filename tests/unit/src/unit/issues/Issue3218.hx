package unit.issues;

@:callable
private abstract A<T>(T) from T {}

class Issue3218 extends Test {
	function test() {
		var a:A<Int->Int> = function(x) return x * 2;
		eq(2, a(1));

		var f:haxe.Constraints.Function = function(){ return 12; }
        eq(12, f());
	}
}