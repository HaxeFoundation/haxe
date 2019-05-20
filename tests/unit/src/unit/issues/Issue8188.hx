package unit.issues;

class Issue8188 extends unit.Test {
	function test() {
		t(new Dummy().checkClass());
		t(new DummyChild().checkClass());

		t(new IDummyInst().checkInterface());
		t(new IDummyChildInst().checkInterface());

		t(new ADummy().checkAbstract());
	}

	static function checkClass(d:Dummy):Bool {
		return true;
	}

	static function checkInterface(d:IDummy):Bool {
		return true;
	}

	static function checkAbstract(d:ADummy):Bool {
		return true;
	}
}

@:using(unit.issues.Issue8188)
private class Dummy {
	public function new() {}
}

private class DummyChild extends Dummy {}

@:using(unit.issues.Issue8188)
private interface IDummy {}
private interface IDummyChild extends IDummy {}

private class IDummyInst implements IDummy {
	public function new() {}
}

private class IDummyChildInst implements IDummyChild {
	public function new() {}
}

@:using(unit.issues.Issue8188)
private abstract ADummy(String) {
	public inline function new() {
		this = '';
	}
}