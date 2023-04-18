package unit.issues;

class Issue11007 extends Test {
	function test() {
		var a:Dynamic<Dynamic> = {day: "numeric"};
		eq("numeric", a.day);
	}
}
