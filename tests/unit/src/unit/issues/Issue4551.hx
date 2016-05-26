package unit.issues;

class Issue4551 extends Test {
	#if js
	function test() {
		try {
			try throw "yay!" catch (e:Dynamic) js.Lib.rethrow();
		} catch (e:Dynamic) {
			eq(e, "yay!");
		}
	}
	#end
}
