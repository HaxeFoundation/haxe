
package unit;

class TestPython extends Test {

	public function testDoWhileAsExpression () {
		var x = 1;
		var z = function () return (do {
			x++;
		} while (x < 3));

		z();

		eq(3, x);
	}

}