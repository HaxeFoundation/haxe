package unit.issues;

class Issue8791 extends unit.Test {
	function test() {
		var a = write(true);
		eq('', a);
	}

	static inline function write(binary = true) {
		return '';
	}
}