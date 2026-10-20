class Main {
	// Inference cycle between fields: reported.
	static function even(n:Int) return n == 0 ? true : odd(n - 1);
	static function odd(n:Int) return n == 0 ? false : even(n - 1);

	// Annotated, self-recursive, or without `return <expr>` (syntactic Void):
	// not reported.
	static function even2(n:Int):Bool return n == 0 ? true : odd2(n - 1);
	static function odd2(n:Int) return n == 0 ? false : even2(n - 1);
	static function fact(n:Int) return n <= 1 ? 1 : n * fact(n - 1);
	static function walkA(n:Int) { if (n > 0) walkB(n - 1); }
	static function walkB(n:Int) { if (n > 0) walkA(n - 1); }

	// Arguments typed by a literal default value: not reported.
	static function indentA(n:Int, tabs = "") { if (n > 0) indentB(n - 1, tabs + "\t"); }
	static function indentB(n:Int, tabs = "") { if (n > 0) indentA(n - 1, tabs); }

	// A default value that only gives part of the type: reported.
	static function partialA(n:Int, v = null) { if (n > 0) partialB(n - 1, v); }
	static function partialB(n:Int, v = null) { if (n > 0) partialA(n - 1, v); }

	static function main() {
		walkA(3);
		indentA(2);
		partialA(2, 1);
		trace(even(4), even2(4), fact(3), A.count() + B.count());
	}
}
