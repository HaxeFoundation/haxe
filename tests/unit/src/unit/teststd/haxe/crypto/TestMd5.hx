package unit.teststd.haxe.crypto;

class TestMd5 extends unit.Test {
	public function test() {
		eq(haxe.crypto.Md5.encode(""), "d41d8cd98f00b204e9800998ecf8427e");
		eq(haxe.crypto.Md5.encode("The quick brown fox jumps over the lazy dog"), "9e107d9d372bb6826bd81d3542a419d6");
		// a String is hashed as its UTF-8 bytes
		eq(haxe.crypto.Md5.encode("héllo wörld €"), "4c214b3ff3f857948d6e94f3c4bea9be");
		eq(haxe.crypto.Md5.make(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")).toHex(), "9e107d9d372bb6826bd81d3542a419d6");
		var all = haxe.io.Bytes.alloc(256 * 3 + 1);
		for (i in 0...all.length)
			all.set(i, i & 255);
		eq(haxe.crypto.Md5.make(all).toHex(), "b636a16c6348f8dfbd5dd21b8b94c9f1");
	}
}
