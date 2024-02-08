package unit.issues;

private function g() {
	return true;
}

class Issue9777 extends unit.Test {
	function test() {
		t(g());
	}
}