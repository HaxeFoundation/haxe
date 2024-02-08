package unit.issues;

class Issue9046 extends unit.Test {
	function test() {
		var a = Utils9046.flatten('hello');
		aeq(['hello'], a);

		//check multiple calls with the same type params
		var a = Utils9046.flatten('hello');
		aeq(['hello'], a);

		//Check it gets a separate module.
		//This test should not rely on a generated module name,
		//but I don't know how to check it without the name.
		t(null != Type.resolveClass('unit.issues.Utils9046_flatten_String'));
	}
}

@:genericClassPerMethod
class Utils9046 {
	@:pure(false)
	@:generic public static function flatten<T>(i:T):Array<T> {
		return [i];
	}
}