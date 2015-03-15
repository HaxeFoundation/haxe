package unit.issues;

private abstract Callback<T>(T->Void) from (T->Void) {}

class Issue2881 extends Test {
	function test() {
		var cb:Callback<Int> = null;
	}
}