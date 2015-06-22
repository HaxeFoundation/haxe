package unit.issues;

class Issue4295 extends Test {
	function test() {

		var dc:Dynamic<Array<Dynamic>> = {a: [0]};
		eq(dc.a[0], 0);
		eq(dc.a[0] = 1, 1);
	}
}
