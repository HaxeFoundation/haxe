package unit.issues;

import utest.Assert;

private enum Ordering {
	EQ;
	LT;
	GT;
}

@:publicFields
private enum abstract TriState(Ordering) from Ordering {
	var True = GT;
	var False = LT;
	var Both = EQ;

	function icon():Int {
		return switch (this : TriState) {
			case True: 1;
			case False: 2;
			case Both: 3;
		}
	}
}

class Issue11213 extends unit.Test {
	function test() {
		Assert.pass();
	}
}
