package unit;

class TestNull extends Test {
	var ni:Null<Int> = null; // a field to prevent local optimizations

	function test() {
		f(ni == 0);
		f(0 == ni);
	}
}
