package;

class Main {
	public static function main() {
		// Converts to
		// trace("Hello, World");
		"Hello, World!" |> trace;

		// Converts to
		// trace("The result is ", Math.sqrt(double(18)));
		18 |> double |> Math.sqrt |> trace("The result is: ");
	}

	static function double(input: Int) return input * 2;
}
