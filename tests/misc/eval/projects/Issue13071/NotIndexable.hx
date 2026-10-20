// #13071: an abstract with a single `from` type leading to an array, which has neither an @:arrayAccess function
// nor a length, can't be read by index. The errors are the typer's for `e[i]` and `e.length` as before, but the
// matcher now reports the one for `e[i]` once per subject instead of once per element read.
abstract Opaque(Array<Int>) from Array<Int> {}

class NotIndexable {
	static function main() {
		var v:Opaque = [1, 2];
		switch v {
			case [a, b]:
			case _:
		}
	}
}
