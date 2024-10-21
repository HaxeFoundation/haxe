
class TestBase extends utest.Test {
	var loop:Loop;

	function setup() {
		loop = Loop.init();
	}

	function teardown() {
		loop.close();
	}
}