package unit.issues;

class Issue6503 extends unit.Test {
	function test() {
		var x = () -> ('hi':Dynamic);
		voidJob(x);
		noAssert();
	}

	@:pure(false) static function voidJob(cb:()->Void) {}
}