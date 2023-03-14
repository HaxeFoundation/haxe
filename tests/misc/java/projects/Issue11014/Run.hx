class Run {
	static function main() {
		var result = test.Main.performMathOperation(test.Main.add);
		trace('Add: ${result}');

		result = test.Main.performMathOperation(test.Main.subtract);
		trace('Subtract: ${result}');

		result = test.Main.performMathOperation(multiply);
		trace('Multiply: ${result}');

		result = test.Main.performMathOperation(function(a, b):Int {
			return Std.int(a / b);
		});
		trace('Divide: ${result}');
	}

	static function multiply(a, b):Int {
		return a * b;
	}
}
