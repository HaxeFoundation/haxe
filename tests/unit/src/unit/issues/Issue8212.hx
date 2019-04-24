package unit.issues;

import haxe.io.Bytes;

class Issue8212 extends unit.Test {
	function test() {
		var hex = "010A7F0AC2800ADFBF0AE0A0800AED9FBF0AEE80800AEFBFBD0AF09F9882F09F9884F09F98990AC8A70A";
		var bytes = Bytes.ofHex(hex);
		eq(hex.length, bytes.length);
		eq(hex, bytes.toHex());
	}
}