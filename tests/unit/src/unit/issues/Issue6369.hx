package unit.issues;

class Issue6369 extends unit.Test {
	function test() {
		eq('${({}) + 'whoops'}', '${({}) + 'whoops'}'); // we only care about this parsing
	}
}