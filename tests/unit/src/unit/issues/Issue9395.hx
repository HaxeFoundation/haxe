package unit.issues;

private class DisposeThing {
	@:generic
	static public function d<T:{function dispose():String;}>(o:T):() -> String {
		return o.dispose;
	}

	public function new() {}

	public function dispose() {
		return "Disposing";
	}
}

class Issue9395 extends Test {
	function test() {
		var dispose = DisposeThing.d(new DisposeThing());
		eq("Disposing", dispose());
	}
}
