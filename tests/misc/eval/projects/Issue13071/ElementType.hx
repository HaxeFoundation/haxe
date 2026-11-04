// #13071: the elements of an abstract which array patterns read by its @:arrayAccess getter have the getter's type,
// while their own patterns are typed against the abstract's `from` type. If the getter's type can't be read by index,
// the error is reported at the switch subject, where the typer's error for `e[i]` was reported before.
abstract Opaque(Array<Int>) from Array<Int> {}

abstract Rows(Array<Array<Int>>) from Array<Array<Int>> {
	public var length(get, never):Int;

	function get_length():Int
		return this.length;

	@:arrayAccess function get(i:Int):Opaque
		return this[i];
}

class ElementType {
	static function main() {
		var v:Rows = [[1]];
		switch v {
			case [[x]]:
			case _:
		}
	}
}
