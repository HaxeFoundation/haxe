package unit.issues;

class Issue8986 extends unit.Test {
	function test() {
		try invoke(null)
		catch(_:Dynamic) {}
		noAssert();
	}

	static inline function invoke(f:(item:Int)->Void) {
		f(1);
	}
}