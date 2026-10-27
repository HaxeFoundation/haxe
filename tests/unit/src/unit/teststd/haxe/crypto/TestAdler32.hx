package unit.teststd.haxe.crypto;

import haxe.crypto.Adler32;
import haxe.io.Bytes;

class TestAdler32 extends unit.Test {

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
		eq(Adler32.make(Bytes.ofString("")), 1);
		eq(Adler32.make(Bytes.ofString("The quick brown fox jumps over the lazy dog")), 0x5BDC0FDA);
		eq(new Adler32().toString(), "0000000000000001");

		eq(Adler32.make(filled(1)), 0x01000100);
		eq(Adler32.make(filled(3799)), 0x0D27C8FC);
		eq(Adler32.make(filled(3800)), 0xD722C9FB);
		eq(Adler32.make(filled(3801)), 0xA22BCAFA);
		eq(Adler32.make(filled(7600)), 0x74FC9404);
		eq(Adler32.make(filled(7601)), 0x0A0E9503);

		eq(Adler32.make(pattern(10000, 31, 7)), 0x41977596);
	}

	public function testSplitUpdate() {
		var b = pattern(9000, 17, 3);
		var a = new Adler32();
		a.update(b, 0, 100);
		a.update(b, 100, 5000);
		a.update(b, 5100, 3900);
		eq(a.get(), 0x527080C4);
	}

	public function testOffset() {
		var b = Bytes.ofString("xxThe quick brown fox jumps over the lazy dogyy");
		var a = new Adler32();
		a.update(b, 2, 43);
		eq(a.get(), 0x5BDC0FDA);
	}
}
