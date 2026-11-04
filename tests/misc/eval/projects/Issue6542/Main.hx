class Main {
	static function main() {
		function isEven(value:Float) return value % 2 == 0;
		for (value in -10 ... 10) {
			switch value {
				case value = isEven(_) => true if (_ > 0):  // runtime error
					trace(value);
				case _:
			}
		}
	}
}