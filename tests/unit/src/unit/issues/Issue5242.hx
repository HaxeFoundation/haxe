package unit.issues;

class Issue5242 extends unit.Test {
	function test() {
		state = 0;
		var buf = new StringBuf();
		for (i in 0...5) {
			switch (next()) {
				case "0":
					buf.add("0");
				case "1":
					buf.add("1");
				case "2":
					buf.add("2");
				default:
					buf.add("" + state);
			}
		}
		eq("01245", buf.toString());
	}

	function test2() {
		state = 0;
		var buf = new StringBuf();
		for (i in 0...5) {
			var v = next();
			switch (v) {
				case "0":
					buf.add("0");
				case "1":
					buf.add("1");
				case "2":
					buf.add("2");
				default:
					buf.add("" + state);
			}
		}
		eq("01245", buf.toString());
	}

	static var state = 0;

	static function next() {
		return "" + state++;
	}

}