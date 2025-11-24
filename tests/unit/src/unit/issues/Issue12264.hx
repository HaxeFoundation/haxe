package unit.issues;

class Issue12264 extends Test {
	#if !eval
	function test() {
		#if (cpp || jvm || hl)
		eq(true, Type.typeof(0i64).match(TInt));
		#else
		eq(true, Type.typeof(0i64).match(TClass(_)));
		#end
	}
	#end
}
