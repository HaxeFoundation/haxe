package unit.issues;

import haxe.io.Bytes;
import haxe.io.Float32Array;
import haxe.io.Float64Array;

class Issue5273 extends unit.Test {
	function testFloat64() {
		var bytes = Bytes.alloc(8);
		var f64 = Float64Array.fromBytes(bytes);
		f64[0] = 1.0;
		eq("000000000000f03f", bytes.toHex());
		eq(1.0, f64[0]);
	}

	function testFloat32() {
		var bytes = Bytes.alloc(4);
		var f32 = Float32Array.fromBytes(bytes);
		f32[0] = 1.0;
		eq("0000803f", bytes.toHex());
		eq(1.0, f32[0]);
	}
}
