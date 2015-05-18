package issues;

@:analyzer(no_local_dce)
class Issue4223 {
	@:js('4;')
	static function testCeilPos() {
		Math.ceil(3.6);
	}

	@:js('-3;')
	static function testCeilNeg() {
		Math.ceil(-3.6);
	}

	@:js('2147483647;')
	static function testCeilMax() {
		Math.ceil(2147483646.5);
	}

	@:js('-2147483648;')
	static function testCeilMin() {
		Math.ceil(-2147483648.5);
	}

	@:js('Math.ceil(2147483647.5);')
	static function testCeilOver() {
		Math.ceil(2147483647.5);
	}

	@:js('Math.ceil(-2147483649.5);')
	static function testCeilUnder() {
		Math.ceil(-2147483649.5);
	}

	@:js('3;')
	static function testFloorPos() {
		Math.floor(3.6);
	}

	@:js('-4;')
	static function testFloorNeg() {
		Math.floor(-3.6);
	}

	@:js('2147483647;')
	static function testFloorMax() {
		Math.floor(2147483647.5);
	}

	@:js('-2147483648;')
	static function testFloorMin() {
		Math.floor(-2147483647.5);
	}

	@:js('Math.floor(2147483648.5);')
	static function testFloorOver() {
		Math.floor(2147483648.5);
	}

	@:js('Math.ceil(-2147483649.5);')
	static function testFloorUnder() {
		Math.ceil(-2147483649.5);
	}
}