package unit.issues;

class Issue4904 extends Test {
	function test() {
		eq(("A" : TestAbstract), TestAbstract.A);
		eq(("B" : TestAbstract), TestAbstract.B);
		eq((("C" : TestAbstract) : Int), -1);
	}
}

enum
abstract TestAbstract(Int) to Int {
	var A = 0;
	var B = 1;
	private var Invalid = -1;

	@:from public static function fromString(s:String) {
		if (s == "A") return A;
		if (s == "B") return B;
		return Invalid;
	}
}