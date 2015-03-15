package unit.issues;

class Issue3948 extends Test {
	function test() {
		eq("ok:true", throwBool(true));
		eq("ok:false", throwBool(false));
		eq("ok:12", throwInt(12));
		eq("ok:-12", throwInt(-12));
		eq("ok:0", throwInt(0));
	}

	function throwBool(b:Bool) {
		return try {
			throw b;
		} catch(b:Bool) {
			"ok:" + b;
		}
	}

	function throwInt(i:Int) {
		return try {
			throw i;
		} catch(i:Int) {
			"ok:" + i;
		}
	}
}