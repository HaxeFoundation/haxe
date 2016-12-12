package unit.issues;

class Issue4988 extends Test {
	static var value:Dynamic;

	function test() {
		#if !lua
		var d:{i:Null<Int>} = null;
		value = (d.i > 0);
		#end
	}

	function foo(v:Dynamic) {}
}