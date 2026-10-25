/**
	Micro benchmarks of the eval interpreter (docs/macro-jit.md, step 0).

	Run with `--interp` (`haxe interp.hxml`) or as a macro (`haxe macro.hxml`),
	both use the same engine. Every case prints its time in milliseconds and a
	checksum, which must not change from one version of the compiler to the next.
**/
class Point {
	public var x:Int;
	public var y:Int;

	public function new(x:Int, y:Int) {
		this.x = x;
		this.y = y;
	}

	public function add(p:Point):Point {
		return new Point(x + p.x, y + p.y);
	}

	public function len2():Int {
		return x * x + y * y;
	}
}

class Point3 extends Point {
	public var z:Int;

	public function new(x:Int, y:Int, z:Int) {
		super(x, y);
		this.z = z;
	}

	override public function len2():Int {
		return super.len2() + z * z;
	}
}

enum Expr {
	EConst(v:Int);
	EVar(name:String);
	EAdd(a:Expr, b:Expr);
	EMul(a:Expr, b:Expr);
	ENeg(e:Expr);
	EIf(c:Expr, a:Expr, b:Expr);
}

enum Ty {
	TInt;
	TBool;
	TUnknown;
}

class Main {
	static var results:Array<String> = [];

	static function time(name:String, f:Void->Int) {
		var t0 = haxe.Timer.stamp();
		var r = f();
		var dt = Std.int((haxe.Timer.stamp() - t0) * 1000);
		Sys.println(StringTools.rpad(name, " ", 12) + StringTools.lpad("" + dt, " ", 7) + " ms   (" + r + ")");
		return dt;
	}

	static function fib(n:Int):Int {
		return n < 2 ? n : fib(n - 1) + fib(n - 2);
	}

	static function calls() {
		return fib(30);
	}

	static function loops() {
		var s = 0;
		for (i in 0...3000000) {
			s += i & 7;
			if (i % 3 == 0)
				s ^= i;
		}
		var i = 0;
		while (i < 1000000) {
			s = (s + i * 3) | 0;
			i++;
		}
		return s;
	}

	static function floats() {
		var s = 0.;
		for (i in 0...4000000) {
			var f = i * 0.5;
			s += f * f / (f + 1.);
		}
		return Std.int(s / 1000);
	}

	static function objects() {
		var acc = new Point(0, 0);
		var s = 0;
		for (i in 0...500000) {
			var p:Point = (i & 1 == 0) ? new Point(i, 1) : new Point3(1, i, 2);
			acc = acc.add(p);
			s += p.len2() & 0xFFFF;
			acc.x &= 0xFFFF;
			acc.y &= 0xFFFF;
		}
		return s + acc.x + acc.y;
	}

	static function arrays() {
		var a = [];
		for (i in 0...200000)
			a.push(i);
		var s = 0;
		for (k in 0...5)
			for (i in 0...a.length)
				s += a[i] & 0xFF;
		for (x in a)
			s += x & 3;
		a.sort((x, y) -> y - x);
		return s + a[0];
	}

	static function anons() {
		var s = 0;
		var objs:Array<{x:Int, y:Int, name:String}> = [];
		for (i in 0...1000)
			objs.push({x: i, y: i * 2, name: "o" + i});
		for (k in 0...1000)
			for (o in objs) {
				s += o.x + o.y;
				o.x = (o.x + 1) & 0xFFF;
			}
		var d:Dynamic = {a: 1, b: 2};
		for (i in 0...1000000)
			s += d.a + d.b;
		return s;
	}

	static function closures() {
		var s = 0;
		var add = function(x:Int) return s += x;
		for (i in 0...1000000)
			add(i & 15);
		var fs = [for (i in 0...10) (x:Int) -> x * i];
		for (k in 0...100000)
			for (f in fs)
				s += f(k) & 1;
		return s;
	}

	static function strings() {
		var b = new StringBuf();
		for (i in 0...400000) {
			b.add("x");
			b.add(i);
		}
		var str = b.toString();
		var s = 0;
		for (i in 0...400000)
			s += StringTools.fastCodeAt(str, i);
		var parts = str.split("x");
		return s + parts.length + str.indexOf("99999");
	}

	static function maps() {
		var m = new Map<String, Int>();
		for (i in 0...50000)
			m.set("k" + i, i);
		var s = 0;
		for (k in 0...4)
			for (i in 0...50000)
				s += m.get("k" + i);
		var im = new Map<Int, Int>();
		for (i in 0...100000)
			im.set(i, i * 2);
		for (i in 0...100000)
			s += im.get(i);
		return s;
	}

	// an hxsl-like workload: building, type checking and evaluating expressions
	static function build(seed:Int, depth:Int):Expr {
		if (depth == 0)
			return (seed & 1) == 0 ? EConst(seed & 15) : EVar("v" + (seed & 3));
		return switch (seed % 4) {
			case 0: EAdd(build(seed * 7 + 1, depth - 1), build(seed * 3 + 2, depth - 1));
			case 1: EMul(build(seed * 5 + 3, depth - 1), EConst(2));
			case 2: ENeg(build(seed + 11, depth - 1));
			default: EIf(EConst(seed & 1), build(seed * 13 + 1, depth - 1), build(seed * 17 + 5, depth - 1));
		}
	}

	static function typeOf(e:Expr, vars:Map<String, Ty>):Ty {
		return switch (e) {
			case EConst(_): TInt;
			case EVar(n): var t = vars.get(n); t == null ? TUnknown : t;
			case EAdd(a, b) | EMul(a, b):
				switch [typeOf(a, vars), typeOf(b, vars)] {
					case [TInt, TInt]: TInt;
					default: TUnknown;
				}
			case ENeg(e): typeOf(e, vars);
			case EIf(c, a, b):
				typeOf(c, vars);
				var ta = typeOf(a, vars);
				ta == typeOf(b, vars) ? ta : TUnknown;
		}
	}

	static function evalExpr(e:Expr, env:{v0:Int, v1:Int, v2:Int, v3:Int}):Int {
		return switch (e) {
			case EConst(v): v;
			case EVar("v0"): env.v0;
			case EVar("v1"): env.v1;
			case EVar("v2"): env.v2;
			case EVar(_): env.v3;
			case EAdd(a, b): (evalExpr(a, env) + evalExpr(b, env)) & 0xFFFF;
			case EMul(a, b): (evalExpr(a, env) * evalExpr(b, env)) & 0xFFFF;
			case ENeg(e): -evalExpr(e, env) & 0xFFFF;
			case EIf(c, a, b): evalExpr(c, env) != 0 ? evalExpr(a, env) : evalExpr(b, env);
		}
	}

	static function shaders() {
		var vars = ["v0" => TInt, "v1" => TInt, "v2" => TInt];
		var s = 0;
		for (i in 0...10000) {
			var e = build(i, 10);
			if (typeOf(e, vars) == TInt)
				s++;
			s += evalExpr(e, {v0: i, v1: 2, v2: 3, v3: 4});
			s &= 0xFFFFFF;
		}
		return s;
	}

	public static function run() {
		var total = 0;
		total += time("calls", calls);
		total += time("loops", loops);
		total += time("floats", floats);
		total += time("objects", objects);
		total += time("arrays", arrays);
		total += time("anons", anons);
		total += time("closures", closures);
		total += time("strings", strings);
		total += time("maps", maps);
		total += time("shaders", shaders);
		Sys.println(StringTools.rpad("total", " ", 12) + StringTools.lpad("" + total, " ", 7) + " ms");
	}

	static function main() {
		run();
	}
}
