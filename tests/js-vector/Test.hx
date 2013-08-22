import haxe.ds.Vector;
import haxe.io.*;
class Test {
	static function main() {
		var v = new Vector<Float>(10);
		for(i in 0...10)
			v[i] = Math.sqrt(i);
		trace(v);
		var b = Bytes.ofString("Hello, world!");
		trace(b.toString());
		var ov = new Vector<Int>(20);
		for(i in 0...20)
			ov[i] = i;
		trace(ov);
		var sv = new Vector<{name: String, value:Int}>(5);
		for(i in 0...sv.length)
			sv[i] = {name: "???", value: Std.random(10)};
		trace(sv);
	}
}