// #13071: Dynamic<T> can't be accessed by index, neither by the typer nor by array patterns. The typer's error for
// `e[i]` used to be reported at the switch subject, once per element read; it is now reported once, at the first
// array pattern.
class DynamicOfT {
	static function main() {
		var v:Dynamic<Int> = {};
		switch v {
			case [a]:
			case _:
		}
	}
}
