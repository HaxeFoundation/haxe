// Haxe 6 field typing rules (docs/incremental-compilation.md, phases 1 and 3.3):
// a field's signature is known from its declaration when it can be.
class Main {
	// A variable initialized with a Dynamic expression is Dynamic: its first
	// use (a call below) no longer binds its type.
	static var dyn = (null : Dynamic);

	// No `return <expr>`: the return type is Void without typing the body.
	static function noReturn() {
		var x = 1;
	}

	// Arguments typed by a literal default value; `null` only gives part of
	// the type, the rest comes from the body.
	static function defaults(tabs = "", n = 0, f = 1.5, b = false, neg = -1, ?v = null) {
		v = n;
	}

	static function main() {
		if (dyn != null) dyn(1);
		$type(dyn);
		$type(noReturn);
		$type(defaults);
	}
}
