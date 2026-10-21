// #13071: an abstract with a single `from` type leading to Dynamic, which converts to Dynamic but has no length,
// can't be read by index. The error used to be "Incompatible pattern"; the array pattern now reports the missing
// length.
abstract DynamicValue(Dynamic) from Dynamic to Dynamic {}

class ToDynamic {
	static function main() {
		var v:DynamicValue = [1, 2];
		switch v {
			case [a]:
			case _:
		}
	}
}
