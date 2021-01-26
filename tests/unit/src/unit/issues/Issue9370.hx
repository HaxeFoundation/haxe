package unit.issues;

class Issue9370 extends unit.Test {
	function test() {
		var foo = 123;
		eq("true  123 ", '${true /* breakme */} ${' ${foo} '}');
		eq("true  123 ", '${true} ${' ${foo} '}');
	}
}