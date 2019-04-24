package unit.issues;

import haxe.io.Bytes;

class Issue8212 extends unit.Test {
	function test() {
		var hex = "010a7f0ac2800adfbf0ae0a0800aed9fbf0aee80800aefbfbd0af09f9882f09f9884f09f98990ac8a70a";
		var bytes = Bytes.ofHex(hex);
		eq(hex.length, bytes.length * 2);
		eq(hex, bytes.toHex());
	}
}