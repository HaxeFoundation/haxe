import Recursive;
import CoreTypes;

class Main {
	static function main() {
		var m = new Map<String, Int>();
		m.set("a", 1);
		var s = haxe.Serializer.run({ a : [1, 2, 3], b : "x", c : m });
		trace(haxe.Unserializer.run(s) != null);
		trace(haxe.Json.stringify({ x : 1.5, y : [true, false], z : null }));
		trace(new EReg("a(b+)", "g").replace("abbbab", "$1"));
		trace(haxe.crypto.Md5.encode("hello"));
		trace(ReachDirectly.run([1, 2, 3]) != null);
		ReachThroughTypedef.a0(null);
		trace(CatchAll.run([1, 2, 3]));
		trace(UseDynamic.a0(1));
	}
}
