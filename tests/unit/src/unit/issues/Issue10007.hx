package unit.issues;

class Issue10007 extends unit.Test {
	function test() {
		final ctor = Abstract.new;
		var o = ctor();
		t(o.isImpl());
	}
}

@:forward.new
private abstract Abstract(Impl) {
	public function isImpl():Bool
		return Std.is(this, Impl);
}

private class Impl {
	public function new() {}
}