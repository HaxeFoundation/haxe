package unit.issues;

class Issue4608 extends Test {
	function test() {
		var s = "𩸽あëa";
		var b = false;
		switch s {
		case "𩸽あëa":
			b = true;
		case "a":
		case _:
		}
		t(b);
	}
}
