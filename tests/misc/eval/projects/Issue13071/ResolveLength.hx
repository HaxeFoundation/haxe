// #13071: array patterns read a value by index the way `for` loops do (ArrayAccess.get_array_access). An abstract
// with a single `from` type leading to an array, whose `length` only comes from @:resolve, was matched before by
// typing `e.length` and `e[i]`; the array pattern now reports the missing Int length.
abstract Indexed(Array<Int>) from Array<Int> {
	@:arrayAccess function get(i:Int):Int
		return this[i];

	@:op(a.b) function resolve(name:String):Int
		return this.length;
}

class ResolveLength {
	static function main() {
		var v:Indexed = [1, 2];
		switch v {
			case [a, b]:
			case _:
		}
	}
}
