// #13071: array patterns read a value by index the way `for` loops do (ArrayAccess.get_array_access), which
// requires an Int length. An abstract with a single `from` type leading to an array, whose `length` is a Float,
// was matched before by typing `e.length` and `e[i]`; its array patterns are now an error.
abstract FloatLengthArray(Array<Int>) from Array<Int> {
	public var length(get, never):Float;

	function get_length():Float
		return this.length;

	@:arrayAccess function get(i:Int):Int
		return this[i];
}

class FloatLength {
	static function main() {
		var v:FloatLengthArray = [1, 2];
		switch v {
			case [a, b]:
			case _:
		}
	}
}
