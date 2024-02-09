class Main {
	static function main() {
		throw new haxe.macro.Expr.Error("hi", (macro _).pos);
	}
}