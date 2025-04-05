package unit.issues;

@:keep
private class PointerData<T> {
	public var get:() -> T;

	public function new(?get:Void->T) {
		if (get == null)
			get = () -> throw "null pointer dereference";
	}

	public var hasSet:Bool = false;
}

class Issue12067 extends Test {
	function test() {
		eq(false, new PointerData().hasSet);
	}
}
