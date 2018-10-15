package unit.issues;

class Issue7443 extends unit.Test {
	function test () {
		eq('true,false', [true, false].join(','));
	}
}