package unit.issues;

using unit.issues.Issue9554;

class Issue9554 extends unit.Test {
	public function test() {
		var obj = merge({foo: 12}, {bar: "bar"});
		obj.staticExtension();
		utest.Assert.pass();
	}

	static function merge<A:{}, B:{}, C:A & B>(a:A, b:B):C {
		return null;
	}

	static function staticExtension(a:{foo:Int, bar:String}) {}
}