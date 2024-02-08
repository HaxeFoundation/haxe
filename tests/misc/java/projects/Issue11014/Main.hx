interface MathOperation {
	function perform(a:Int, b:Int):Int;
}

class Ops {
	static public final add:MathOperation = (a, b) -> a + b;
	static public final subtract:MathOperation = (a, b) -> a - b;

	static public function performMathOperation(operation:MathOperation) {
		return operation.perform(8, 4);
	}
}

class Main {
	static function main() {
		var result = Ops.performMathOperation(Ops.add);
		trace('Add: ${result}');

		result = Ops.performMathOperation(Ops.subtract);
		trace('Subtract: ${result}');

		result = Ops.performMathOperation(multiply);
		trace('Multiply: ${result}');

		result = Ops.performMathOperation(function(a, b):Int {
			return Std.int(a / b);
		});
		trace('Divide: ${result}');
	}

	static function multiply(a, b):Int {
		return a * b;
	}
}