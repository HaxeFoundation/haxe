// #13071: an abstract with a single `from` type leading to an array, which has a length but neither an @:arrayAccess
// function nor a conversion to an array, can't be read by index. The typer's error for `e[i]` used to be reported at
// the switch subject, once per element read; the array pattern now reports that the elements can't be read.
abstract Opaque(Array<Int>) from Array<Int> {
	public var length(get, never):Int;

	function get_length():Int
		return this.length;
}

class NotIndexable {
	static function main() {
		var v:Opaque = [1, 2];
		switch v {
			case [a, b]:
			case _:
		}
	}
}
