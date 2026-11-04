// Reports which modules made it into the hxb archive produced by the previous
// compilation step. With `exclude: ["pack.**"]`, everything under the `pack`
// package (at any depth) must be absent, while `Main` and `other.C` remain. A
// literal (non-wildcard) match of `pack.**` would exclude nothing, so this also
// guards that the wildcard is actually interpreted.
//
// Entry names are stored as plain ASCII in the zip headers, so a raw byte
// search over the archive is enough (and avoids depending on the exact zip
// features the writer uses).
import haxe.io.Bytes;

class Check {
	static function contains(haystack:Bytes, needle:Bytes):Bool {
		var max = haystack.length - needle.length;
		for (i in 0...max + 1) {
			var found = true;
			for (j in 0...needle.length) {
				if (haystack.get(i + j) != needle.get(j)) {
					found = false;
					break;
				}
			}
			if (found)
				return true;
		}
		return false;
	}

	static function main() {
		var data = sys.io.File.getBytes("bin/hxb.zip");
		function present(entry:String):Bool {
			return contains(data, Bytes.ofString(entry));
		}
		Sys.println("Main present: " + present("/Main.hxb"));
		Sys.println("pack.A present: " + present("/pack/A.hxb"));
		Sys.println("pack.sub.B present: " + present("/pack/sub/B.hxb"));
		Sys.println("other.C present: " + present("/other/C.hxb"));
	}
}
