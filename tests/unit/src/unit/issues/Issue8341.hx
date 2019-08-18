package unit.issues;

class Issue8341 extends unit.Test {
	static var list(default, never):CustomArray = [];

	function test() {
		list += 10;
		aeq([10], list);
	}
}

abstract CustomArray(Array<Int>) from Array<Int> to Array<Int> {
	@:op(A+=B) inline function add(value:Int) this.push(value);
}