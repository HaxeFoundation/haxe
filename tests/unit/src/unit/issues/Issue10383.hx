package unit.issues;

class Issue10383 extends Test {
	function test() {
		exec();
		utest.Assert.pass();
	}

	static inline function exec():{a:Int, ?b:String, ?c:String} {
		if (Math.random() > 0.5) {
			return {a: 0, b: ""};
		}
		return {a: 0, c: ""};
	}
}
