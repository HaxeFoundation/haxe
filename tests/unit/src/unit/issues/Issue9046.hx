package unit.issues;

class Issue9046 extends unit.Test {
	function test() {
		var a = Utils9046.flatten('hello');
		aeq(['hello'], a);

		//Check it gets a separate module.
		//This test should not rely on a generated module name,
		//but I don't know how to check it without the name.
		t(null != Type.resolveClass('unit.issues.Utils9046_flatten_String'));

		//make sure generic method wrapping still works
		var a = Utils9046.flatten(10);
		eq(10, a.length);
	}
}

class Utils9046 {
	@:pure(false)
	@:generic public static function flatten<T>(i:T):Array<T> {
		return [i];
	}

	public static function flatten_Int(i:Int):Array<Int> {
		return [for(_ in 0...i) i];
	}
}