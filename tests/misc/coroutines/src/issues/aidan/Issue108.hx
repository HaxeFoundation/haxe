package issues.aidan;

class Issue108 extends utest.Test {
	public function testCast() {
		var a = [1];
		Assert.equals(1, CoroRun.run(() -> {
			var v = cast if (a.length == 0) {
				null;
			} else {
				a.shift();
			};
			v;
		}));
	}

	public function testParenthesis() {
		var a = [1];
		Assert.equals(1, CoroRun.run(() -> {
			var v = (if (a.length == 0) {
				null;
			} else {
				a.shift();
			});
			v;
		}));
	}

	public function testMetadata() {
		var a = [1];
		Assert.equals(1, CoroRun.run(() -> {
			var v = @:myMeta if (a.length == 0) {
				null;
			} else {
				a.shift();
			};
			v;
		}));
	}
}