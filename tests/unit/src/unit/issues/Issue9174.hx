package unit.issues;

class Issue9174 extends unit.Test {
	function test() {
		var value = false;
		try {
			try {
				throw '';
			} catch(e:String) {
				value = true;
				t(value);
				throw e;
			}
		} catch(e:String) {
			t(value);
		}
	}
}
