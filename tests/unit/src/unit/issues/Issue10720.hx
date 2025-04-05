package unit.issues;

class Issue10720 extends unit.Test {
	function test() {
		eq(1, switcheroo("😀 😀"));
		eq(2, switcheroo("名 字"));
	}

	function switcheroo(x:String) {
		return switch (x) {
			case '😀 😀':
				1;
			case '名 字':
				2;
			case _:
				0;
		}
	}
}
