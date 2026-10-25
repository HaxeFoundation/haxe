package cases;

/** Enum construction and pattern matching on an expression tree. **/
class Enums {
	static inline function next(seed:Int, a:Int, b:Int) {
		return (seed * a + b) & 0xFFFFF;
	}

	static function build(depth:Int, seed:Int):Expr {
		if (depth == 0)
			return seed % 3 == 0 ? EVar(seed % 4) : EConst(seed % 10);
		return switch (seed % 4) {
			case 0: EAdd(build(depth - 1, next(seed, 7, 1)), build(depth - 1, next(seed, 3, 2)));
			case 1: EMul(build(depth - 1, next(seed, 5, 3)), EConst(2));
			case 2: ENeg(build(depth - 1, next(seed, 1, 1)));
			case _: EIf(build(depth - 1, next(seed, 11, 1)), build(depth - 1, next(seed, 1, 5)), EConst(1));
		}
	}

	static function eval(e:Expr, env:Array<Int>):Int {
		return switch (e) {
			case EConst(v): v;
			case EVar(i): env[i];
			case EAdd(a, b): (eval(a, env) + eval(b, env)) & 0xFFFF;
			case EMul(a, b): (eval(a, env) * eval(b, env)) & 0xFFFF;
			case ENeg(a): -eval(a, env) & 0xFFFF;
			case EIf(c, a, b): eval(c, env) & 1 == 0 ? eval(a, env) : eval(b, env);
		}
	}

	static function simplify(e:Expr):Expr {
		return switch (e) {
			case EAdd(EConst(0), a) | EAdd(a, EConst(0)): simplify(a);
			case EMul(EConst(1), a) | EMul(a, EConst(1)): simplify(a);
			case ENeg(ENeg(a)): simplify(a);
			case EAdd(a, b): EAdd(simplify(a), simplify(b));
			case EMul(a, b): EMul(simplify(a), simplify(b));
			case ENeg(a): ENeg(simplify(a));
			case EIf(c, a, b): EIf(simplify(c), simplify(a), simplify(b));
			case _: e;
		}
	}

	public static function run() {
		var sum = 0;
		for (k in 0...30) {
			var e = simplify(build(12, k));
			for (i in 0...5)
				sum = (sum + eval(e, [i, k, i + k, 1])) & 0xFFFFFF;
		}
		return sum;
	}
}

private enum Expr {
	EConst(v:Int);
	EVar(i:Int);
	EAdd(a:Expr, b:Expr);
	EMul(a:Expr, b:Expr);
	ENeg(a:Expr);
	EIf(c:Expr, a:Expr, b:Expr);
}
