package unit.issues;

class Issue12264 extends Test {
	#if !eval
	function test() {
		eq(true, Type.typeof(0i64).match(TInt64));
	}
	#end
}
