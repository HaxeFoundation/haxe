package unit.issues;

class Issue11402 extends Test {
	static function binopWithReturn() {
		var ans = Std.string(return "") + "";
	}

	static function binopWithThrow() {
		var ans = Std.string(throw "") + "";
	}

	static function binopWithBreak() {
		while (true) {
			var ans = Std.string(break) + "";
			return "b";
		}
		return "a";
	}

	static function binopWithContinue() {
		var a = 0;
		while (true) {
			if (a++ > 0) {
				return "a";
			}
			var ans = Std.string(continue) + "";
			return "b";
		}
	}

	static function arrayWithReturn() {
		var ans = [Std.string(return "")][0];
	}

	static function arrayWithThrow() {
		var ans = [Std.string(throw "")][0];
	}

	static function arrayWithBreak() {
		while (true) {
			var ans = [Std.string(break)][0];
			return "b";
		}
		return "a";
	}

	static function arrayWithContinue() {
		var a = 0;
		while (true) {
			if (a++ > 0) {
				return "a";
			}
			var ans = [Std.string(continue)][0];
			return "b";
		}
	}

	static function originalExample() {
		var cells = [1, 2, 3];
		var ans = "";
		ans += '${cells.length} cell${if (cells.length == 1) {return "";} else {return "s";}}.\n';
		return ans;
	}

	function test() {
		eq("", binopWithReturn());
		exc(binopWithThrow);
		eq("a", binopWithBreak());
		eq("a", binopWithContinue());

		eq("", arrayWithReturn());
		exc(arrayWithThrow);
		eq("a", arrayWithBreak());
		eq("a", arrayWithContinue());

		eq("s", originalExample());
	}
}
