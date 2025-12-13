package unit.issues;

class Issue12264 extends Test {
	function test() {
		eq(true, Type.typeof(256i64).match(TInt64));
	}
}
