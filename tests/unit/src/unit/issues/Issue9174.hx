package unit.issues;

private function doThrow() {
	throw "from doThrow";
}

class Issue9174 extends unit.Test {
	function test() {
		var value = "";
		try {
			try {
				throw "from throw";
			} catch (e:String) {
				value += "inner catch";
				throw e;
			}
		} catch (e:String) {
			value += ", outer catch, " + e;
		}
		eq("inner catch, outer catch, from throw", value);
	}

	function test2() {
		var value = "";
		try {
			try {
				doThrow();
			} catch (e:String) {
				value += "inner catch";
				throw e;
			}
		} catch (e:String) {
			value += ", outer catch, " + e;
		}
		eq("inner catch, outer catch, from doThrow", value);
	}
}
