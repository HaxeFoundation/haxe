package unit.teststd.haxe.crypto;

import haxe.crypto.Adler32;
import haxe.io.Bytes;

class TestAdler32 extends unit.Test {

	// Compare as hex: the size of Int differs between targets
	static inline function hex(i:Int)
		return StringTools.hex(i, 8);

	static function filled(len:Int) {
		var b = Bytes.alloc(len);
		b.fill(0, len, 0xFF);
		return b;
	}

	static function pattern(len:Int, mul:Int, add:Int) {
		var b = Bytes.alloc(len);
		for (i in 0...len)
			b.set(i, (i * mul + add) & 0xFF);
		return b;
	}

	public function test() {
		eq(1, Adler32.make(Bytes.ofString("")));
		eq("5BDC0FDA", hex(Adler32.make(Bytes.ofString("The quick brown fox jumps over the lazy dog"))));
		eq("0000000000000001", new Adler32().toString());

		eq("01000100", hex(Adler32.make(filled(1))));
		eq("0D27C8FC", hex(Adler32.make(filled(3799))));
		eq("D722C9FB", hex(Adler32.make(filled(3800))));
		eq("A22BCAFA", hex(Adler32.make(filled(3801))));
		eq("74FC9404", hex(Adler32.make(filled(7600))));
		eq("0A0E9503", hex(Adler32.make(filled(7601))));

		eq("41977596", hex(Adler32.make(pattern(10000, 31, 7))));
	}

	public function testSplitUpdate() {
		var b = pattern(9000, 17, 3);
		var a = new Adler32();
		a.update(b, 0, 100);
		a.update(b, 100, 5000);
		a.update(b, 5100, 3900);
		eq("527080C4", hex(a.get()));
	}

	public function testOffset() {
		var b = Bytes.ofString("xxThe quick brown fox jumps over the lazy dogyy");
		var a = new Adler32();
		a.update(b, 2, 43);
		eq("5BDC0FDA", hex(a.get()));
	}
}
