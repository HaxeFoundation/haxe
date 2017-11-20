package unit.issues;

class Issue2914 extends Test {
	function test() {
		var o:Null<Int> = 0;

		t(unit.HelperMacros.typeError({
			var x = switch o {
				case var x if (x > 1): x;
			}
		}));

		var x = switch o {
			case var x if (x > 1): x;
			case var x: 1;
		}
		eq(1, x);

		switch (o) {
			case var y if (y < 1):
				x = 5;
		}
		eq(5, x);
	}
}