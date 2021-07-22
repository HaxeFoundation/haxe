package unit.issues;

class Issue10018 extends unit.Test {
	function test() {
		eq(0, rest());
	}

	@:pure(false)
	function rest(?arg, ...args:Int):Int {
		return args.length;
	}
}