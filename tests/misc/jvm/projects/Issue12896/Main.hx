// Triggers genjvm emitting a Signature attribute with a primitive type
// (e.g. `I`) as a generic type argument — invalid per JVMS 4.7.9.1; d8/r8
// reject the signature with "Invalid signature ... Parser error: Expected
// L, [ or T".
class Main {
	static function withCmp(cmp:java.util.function.BiFunction<String, String, Int>) {
		return cmp.apply("ab", "a");
	}

	static public function main() {
		// The inline closure becomes a synthetic Closure_* class whose
		// Signature attribute references BiFunction<String,String,Int>.
		// With the bug, the third type arg is emitted as primitive `I`
		// instead of `Ljava/lang/Integer;`, producing `;I>` in the Signature
		// attribute — d8/r8 then reject it.
		trace(withCmp((a, b) -> a.length - b.length));
	}
}
