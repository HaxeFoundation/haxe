package unit.issues;

@:enum
private abstract Enum1(Int) from Int to Int {
	var AA = 16;
}

@:enum
private abstract Enum2(Int) from Int to Int {
	var BShl = Enum1.AA << 1;
	var BShr = Enum1.AA >> 1;
	var BUShr = Enum1.AA >>> 1;
	var BOr = Enum1.AA | 1;
	var BAnd = Enum1.AA & 1;
	var BXor = Enum1.AA ^ 1;
	var BNeg = -Enum1.AA;
	var BNegBits = ~Enum1.AA;
}

class Issue4819 extends Test {
	function test() {
		eq(32, BShl);
		eq(8, BShr);
		eq(8, BUShr);
		eq(17, BOr);
		eq(0, BAnd);
		eq(17, BXor);
		eq(-16, BNeg);
		eq(-17, BNegBits);
	}
}