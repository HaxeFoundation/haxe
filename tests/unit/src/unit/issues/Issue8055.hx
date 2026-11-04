package unit.issues;

class Issue8055 extends Test {
	function test() {
		var dir = "a/b";
		var str = 'file="${dir.split("/").join("\\")}"';
		eq('file="a\\b"', str);
	}
}
