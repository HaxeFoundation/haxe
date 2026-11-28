class Main {
	static function main() {
		// An unknown operand added to a Float or Dynamic value: Haxe 6 infers it as Int.
		function offset(x, scale:Float) return x + scale * 0.5;
		var d:Dynamic = 1.5;
		function shift(y) return d + y;
		var total = 0.0;
		function add(v) total += v;

		// Not reported: annotated, added to an Int (already Int), or to a String (stays open).
		function offset2(x:Float, scale:Float) return x + scale * 0.5;
		function count(n) return n + 1;
		function label(v) return "value: " + v;

		trace(offset(1.5, 2), shift(1), offset2(1.5, 2), count(1), label(1));
		add(1.5);
	}
}
