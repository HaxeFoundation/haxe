package unit.issues;
class Issue6349 extends Test{
	function test() {
		var a = [];
		for (i in 1...5) {
			try {
				if (i== 1) {
					throw null;
				}
				a.push(i);
			}
			catch (_:Dynamic) {
				continue;
			}
		}
		eq(a + '', '[2,3,4]');
	}
}
