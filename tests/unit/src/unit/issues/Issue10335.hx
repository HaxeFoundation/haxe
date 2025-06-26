package unit.issues;

private class NotMain {
	public final prop = {
		var testvalue = 1;
		{
			get: () -> testvalue,
			set: v -> testvalue = v,
		}
	}

	public function new() {}
}

class Issue10335 extends unit.Test {
	function test() {
		final inst = new NotMain();
		eq(1, inst.prop.get());
		inst.prop.set(2);
		eq(2, inst.prop.get());
	}
}
