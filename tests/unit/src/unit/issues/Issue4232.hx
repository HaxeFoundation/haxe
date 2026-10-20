package unit.issues;

@:keep
class Issue4232CreateMe {
	public var value:String;
	public function new(a1:String, a2:String, a3:String, a4:String, a5:String, a6:String, a7:String, a8:String, a9:String, a10:String, a11:String, a12:String, a13:String, a14:String) {
		value = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14;
	}
}

class Issue4232 extends Test {
	#if !hl // too many arguments in HL
	function test() {
		var c:Issue4232CreateMe = Type.createInstance(Issue4232CreateMe, [for (i in 1...15) "" + i]);
		eq("1234567891011121314", c.value);
	}
	#end
}