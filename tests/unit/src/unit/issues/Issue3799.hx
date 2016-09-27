package unit.issues;

class Issue3799 extends Test {

	static var arr:Array<Dynamic> = [0, "Hello", " World", 0.4];

	function test() {
		var d:Dynamic = arr[1] + arr[2];
		eq("Hello World", d);
	}
}