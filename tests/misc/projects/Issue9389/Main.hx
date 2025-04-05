class Main {
	static function main() {
		f(123);
	}

	static macro function f(e) {
		throw new haxe.macro.Expr.Error("boop", e.pos);
	}
}