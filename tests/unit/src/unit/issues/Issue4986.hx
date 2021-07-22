package unit.issues;

class Issue4986 extends Test {
	function test() {
		try {
			var v = new haxe.ds.Vector<Array<Float>>(1);
			foo(v[0].length);
		} catch (e:Dynamic) {}
		noAssert();
	}

	function foo(_) {}
}