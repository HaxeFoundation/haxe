package unit.issues;

class Issue9738 extends Test {
#if cs
	function test() {
		try {
			try {
				throw new haxe.Exception("Hello");
			} catch (e) {
				cs.Lib.rethrow(e);
			}
			assert();
		} catch(e) {
			noAssert();
		}
	}
#end
}