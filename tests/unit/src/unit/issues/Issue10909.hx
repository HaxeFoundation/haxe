package unit.issues;

class Issue10909 extends Test {
	function test() {
		var arr:Array<Any> = [1, 2, 3, null, 5];
		var r = haxe.Rest.of(arr);
		eq(r[0], 1);
		eq(r[3], null);
		eq(r[4], 5);
	}
}
