class Main {
	// Inference cycle between fields: reported.
	static function even(n:Int) return n == 0 ? true : odd(n - 1);
	static function odd(n:Int) return n == 0 ? false : even(n - 1);

	// Annotated or self-recursive: not reported.
	static function even2(n:Int):Bool return n == 0 ? true : odd2(n - 1);
	static function odd2(n:Int) return n == 0 ? false : even2(n - 1);
	static function fact(n:Int) return n <= 1 ? 1 : n * fact(n - 1);

	static function main() {
		trace(even(4), even2(4), fact(3), A.count() + B.count());
	}
}
