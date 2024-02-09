package unit.issues;

class Issue10754 extends Test {
	function test() {
		var a = [0, 1, 2];
		var boundPush = a.push.bind();
		eq(boundPush(3), 4);
	}
}
