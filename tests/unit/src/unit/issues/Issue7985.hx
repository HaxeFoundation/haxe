package unit.issues;

class Issue7985 extends unit.Test {
	function test() {
		gen((null:Rec));
		(null:Gen<Rec>);
		noAssert();
	}

	@:generic
	static function gen<T>(v:T) {}
}

@:generic
private class Gen<T> {}

private typedef Rec = {
	field:Rec
}
