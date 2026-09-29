package unit.issues;

class Issue13055 extends Test {
	#if hl
	function testF32ThenUI16() {
		var b = new hl.Bytes(4);
		b.setF32(0, 1.);
		var tmp = b.getUI16(2);
		b.setF32(0, 2.);
		eq(16256, tmp);
	}

	function testI32ThenUI16() {
		var b = new hl.Bytes(4);
		b.setI32(0, 0x3F800000);
		var tmp = b.getUI16(2);
		b.setI32(0, 0x40000000);
		eq(0x3F80, tmp);
	}

	function testUI16ThenI32() {
		var b = new hl.Bytes(4);
		b.setUI16(0, 0);
		b.setUI16(2, 0x3F80);
		var tmp = b.getI32(0);
		b.setUI16(2, 0x4000);
		eq(0x3F800000, tmp);
	}

	function testF64ThenI32() {
		var b = new hl.Bytes(8);
		b.setF64(0, 1.);
		var tmp = b.getI32(4);
		b.setF64(0, 2.);
		eq(0x3FF00000, tmp);
	}

	function testUnaligned() {
		var b = new hl.Bytes(9);
		b.setF32(1, 1.);
		var tmp = b.getUI16(3);
		b.setF32(1, 2.);
		eq(16256, tmp);
		eq(2., b.getF32(1));
	}
	#end
}
