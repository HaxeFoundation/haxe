package issues;

@:analyzer(no_local_dce)
class Issue4223 {
	@:js('var a = 4;')
	static function testCeilPos() {
		var a = Math.ceil(3.6);
	}

	@:js('var a = -3;')
	static function testCeilNeg() {
		var a = Math.ceil(-3.6);
	}

	@:js('var a = 2147483647;')
	static function testCeilMax() {
		var a = Math.ceil(2147483646.5);
	}

	@:js('var a = -2147483648;')
	static function testCeilMin() {
		var a = Math.ceil(-2147483648.5);
	}

	@:js('var a = Math.ceil(2147483647.5);')
	static function testCeilOver() {
		var a = Math.ceil(2147483647.5);
	}

	@:js('var a = Math.ceil(-2147483649.5);')
	static function testCeilUnder() {
		var a = Math.ceil(-2147483649.5);
	}

	@:js('var a = 3;')
	static function testFloorPos() {
		var a = Math.floor(3.6);
	}

	@:js('var a = -4;')
	static function testFloorNeg() {
		var a = Math.floor(-3.6);
	}

	@:js('var a = 2147483647;')
	static function testFloorMax() {
		var a = Math.floor(2147483647.5);
	}

	@:js('var a = -2147483648;')
	static function testFloorMin() {
		var a = Math.floor(-2147483647.5);
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