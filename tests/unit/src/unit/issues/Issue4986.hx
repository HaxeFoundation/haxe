package unit.issues;

class Issue4986 extends Test {
	function test() {
		try {
			var v = new haxe.ds.Vector<Array<Float>>(1);
			#if cppia //see https://github.com/HaxeFoundation/haxe/issues/9261
			v[0].length;
			#else
			foo(v[0].length);
			#end
		} catch (e:Dynamic) {}
		noAssert();
	}

	function foo(_) {}
}