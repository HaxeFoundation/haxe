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

	// The type read is already fully determined by a first `return`: not reported.
	static function objToString(o:Dynamic) {
		if (o == null) return "null";
		return "{ " + colToString(o) + " }";
	}
	static function colToString(v:Dynamic) {
		if (Std.isOfType(v, Array)) return [for (x in (v:Array<Dynamic>)) objToString(x)].join(", ");
		return Std.string(v);
	}

	// The type read is still open at the recursive call: reported.
	static function objToString2(o:Dynamic) {
		if (o != null) return "{ " + colToString2(o) + " }";
		return "null";
	}
	static function colToString2(v:Dynamic) {
		if (Std.isOfType(v, Array)) return [for (x in (v:Array<Dynamic>)) objToString2(x)].join(", ");
		return Std.string(v);
	}

	// An inline field in the middle of the cycle, whose body is needed to inline it: reported.
	static function down(n:Int) return n == 0 ? 0 : step(n);
	static inline function step(n:Int):Int return down2(n - 1);
	static function down2(n:Int) return down(n);

	// An argument typed by its literal default value counts as declared: not reported.
	public static function show(n:Int) return n == 0 ? "" : Printer.print(n - 1);

	static function main() {
		walkA(3);
		trace(even(4), even2(4), fact(3), A.count() + B.count(), objToString([1]), objToString2([2]), down(3), show(2));
	}
}

class Printer {
	public static function print(n:Int) {
		var p = new Printer();
		p.add(n);
		return p.buf.toString();
	}

	var buf = new StringBuf();

	function new() {}

	function add(n:Int, tabs = "") {
		buf.add(tabs + Main.show(n));
	}
}
