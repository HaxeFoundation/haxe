package unit.teststd.haxe.crypto;

class TestCrc32 extends unit.Test {
	public function test() {
		eq(haxe.crypto.Crc32.make(haxe.io.Bytes.ofString("")), 0);
		eq(haxe.crypto.Crc32.make(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")), 0x414FA339);
	}
}
