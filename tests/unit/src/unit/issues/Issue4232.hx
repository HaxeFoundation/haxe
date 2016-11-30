package unit.issues;

@:keep
class Issue4232CreateMe {
	public var value:String;
	public function new(a1:String, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) {
		value = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
	}
}

class Issue4232 extends Test {
	function test() {
		#if !hl
		var c:Issue4232CreateMe = Type.createInstance(Issue4232CreateMe, [for (i in 1...15) "" + i]);
		eq("1234567891011121314", c.value);
		#end
	}
}