package unit.issues;

class Issue4421 extends Test {
	function test() {
		var buf = haxe.io.UInt8Array.fromArray([for (i in 0...20) i]);
		var match = 0;
		var cur_match = 0;
		var scan_end = 20;

		do {
		  match = cur_match;

		  if (buf[match + 1] != scan_end ||
			  buf[++match]   != buf[scan_end - 1]) {
			continue;
		  }

		} while (((cur_match = buf[++cur_match]) < 10));
		eq(10, cur_match);
	}
}