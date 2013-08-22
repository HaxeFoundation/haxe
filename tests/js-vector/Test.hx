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
		var nvs = new Vector<Int>(6);
		nvs[0] = 9;
		Vector.blit(ov, 10, nvs, 1, 5);
		trace(nvs);
		//sould be 10...14
	}
}