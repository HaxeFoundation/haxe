package unit.issues;

class Issue3732 extends Test {
	function test() {
		var a;
        var b = a = try 1 catch (_:Dynamic) 2;
		eq(1, a);
		eq(1, b);
	}
}