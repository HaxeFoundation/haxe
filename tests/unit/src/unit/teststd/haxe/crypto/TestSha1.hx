package unit.teststd.haxe.crypto;

class TestSha1 extends unit.Test {
	public function test() {
		eq(haxe.crypto.Sha1.encode(""), "da39a3ee5e6b4b0d3255bfef95601890afd80709");
		eq(haxe.crypto.Sha1.encode("The quick brown fox jumps over the lazy dog"), "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12");
		// a String is hashed as its UTF-8 bytes
		eq(haxe.crypto.Sha1.encode("héllo wörld €"), "e8d02b6e2c8ba427cea42c9e54e133943a3486f4");
		eq(haxe.crypto.Sha1.make(haxe.io.Bytes.ofString("The quick brown fox jumps over the lazy dog")).toHex(), "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12");
		var all = haxe.io.Bytes.alloc(256 * 3 + 1);
		for (i in 0...all.length)
			all.set(i, i & 255);
		eq(haxe.crypto.Sha1.make(all).toHex(), "df2b666a77549b51663dc23eae93760d06811bad");
	}
}
