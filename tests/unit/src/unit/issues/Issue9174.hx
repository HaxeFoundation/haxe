package unit.issues;

class Issue9174 extends unit.Test {
	function test() {
		var value = "";
		try {
			try {
				throw '';
			} catch (e:String) {
				value += "inner catch";
				throw e;
			}
		} catch (e:String) {
			value += ", outer catch";
		}
		eq("inner catch, outer catch", value);
	}
}
