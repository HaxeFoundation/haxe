// #13071: array patterns read a value by index the way `for` loops do (ArrayAccess.get_array_access). An abstract
// with a single `from` type leading to an array, whose `length` only comes from a static extension, was matched
// before by typing `e.length` and `e[i]`; its array patterns are now an error.
using UsingLength.LengthTools;

abstract Indexed(Array<Int>) from Array<Int> {
	@:arrayAccess function get(i:Int):Int
		return this[i];
}

class LengthTools {
	public static function length(v:Indexed):Int
		return (cast v : Array<Int>).length;
}

class UsingLength {
	static function main() {
		var v:Indexed = [1, 2];
		switch v {
			case [a, b]:
			case _:
		}
	}
}
