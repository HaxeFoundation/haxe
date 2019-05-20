package unit.issues;

class Issue8188 extends unit.Test {
	function test() {
		t(new Dummy().checkClass());
		t(new DummyChild().checkClass());
	}

	static function checkClass(d:Dummy):Bool {
		return true;
	}
}

@:using(unit.issues.Issue8188)
private class Dummy {
	public function new() {}
}

private class DummyChild extends Dummy {

}