// A conditional, switch, block or try whose value is declared, assigned to a local or
// returned is generated as statements, not as a function made and called on the spot.
class Main {
	static var log:Array<String> = [];

	static function check(name:String, actual:Dynamic, expected:Dynamic) {
		if (actual != expected) {
			Sys.println('$name: expected $expected but got $actual');
		}
	}

	static function pick(b:Bool):Bool {
		log.push(b ? "t" : "f");
		return b;
	}

	static function thrower(b:Bool):Int {
		if (b)
			throw "no";
		return 1;
	}

	static inline function peek(s:String, i:Int):Int {
		final c = s.charCodeAt(i);
		return c == null ? -1 : c;
	}

	static function sign(x:Int):Int {
		return x < 0 ? -1 : x == 0 ? 0 : 1;
	}

	static function name(k:Int):String {
		return switch k {
			case 1: "one";
			case 2: "two";
			case _: "many";
		}
	}

	static function blockValue(x:Int):Int {
		return {
			final y = x * 2;
			y + 1;
		}
	}

	static function tryValue(b:Bool):Int {
		return try thrower(b) catch (e:String) -1;
	}

	static function returnInBranch(b:Bool):Int {
		final x = if (b) return 1 else 2;
		return x + 10;
	}

	static function returnInTry(b:Bool):Int {
		try {
			return b ? 3 : 4;
		} catch (e:Dynamic) {
			return -1;
		}
	}

	static var sink = 0;

	static function assignLocal(b:Bool):Int {
		var x = 0;
		x = b ? 1 : 2;
		return x;
	}

	static function declare(b:Bool):Int {
		final x = b ? 1 : 2;
		return x;
	}

	// Kilobytes allocated by 10000 calls of f, with the collector stopped.
	static function allocated(f:Int->Void):Float {
		lua.Lua.collectgarbage(Collect);
		lua.Lua.collectgarbage(Stop);
		final before:Float = lua.Lua.collectgarbage(Count);
		for (i in 0...10000) {
			f(i);
		}
		final after:Float = lua.Lua.collectgarbage(Count);
		lua.Lua.collectgarbage(Restart);
		return after - before;
	}

	static function checkNoAllocation(name:String, f:Int->Void) {
		final kb = allocated(f);
		// a closure per call would be several hundred kilobytes
		if (kb > 20) {
			Sys.println('$name allocates: $kb KB for 10000 calls');
		}
	}

	static function main() {
		checkNoAllocation("declare", i -> sink += declare(i % 2 == 0));
		checkNoAllocation("assign", i -> sink += assignLocal(i % 2 == 0));
		checkNoAllocation("return", i -> sink += sign(i - 5000));
		checkNoAllocation("return switch", i -> sink += name(i % 3).length);
		checkNoAllocation("inline", i -> {
			final c = peek("ab", i % 3);
			sink += c;
		});

		final t = pick(true);
		final f = pick(false);

		final a = t ? 1 : 2;
		check("var", a, 1);
		final nested = f ? "a" : t ? "b" : "c";
		check("nested", nested, "b");
		final s = switch a {
			case 1: "one";
			case _: "other";
		}
		check("var switch", s, "one");
		final b = {
			final i = 3;
			i * 2;
		}
		check("var block", b, 6);
		final n:Null<Int> = f ? 5 : null;
		final coalesced = n ?? 7;
		check("var ??", coalesced, 7);
		final r = try thrower(true) catch (e:String) -1;
		check("var try", r, -1);

		// The initializer reads an outer variable of the same name.
		final c = 5;
		{
			final c = c > 3 ? c * 2 : -1;
			check("shadowing read", c, 10);
		}
		check("outer kept", c, 5);
		// The initializer declares a variable of the same name.
		final d = {
			final d = 4;
			t ? d + 1 : d;
		}
		check("shadowing var", d, 5);
		// An inline function twice in one function: compiler temporaries of the same name.
		check("peek", peek("AB", 0) + peek("AB", 1) + peek("AB", 2), 65 + 66 - 1);

		var x = 1;
		x = t ? x + 1 : 0;
		check("assign", x, 2);
		x = {
			final x = 7;
			f ? 0 : x;
		}
		check("assign, var of the same name", x, 7);

		log = [];
		final order = {
			log.push("a");
			pick(true) ? {
				log.push("b");
				1;
			} : 2;
		}
		check("order", log.join(","), "a,t,b");
		check("order value", order, 1);

		check("return nested", sign(-4) + sign(0) * 10 + sign(9) * 100, 99);
		check("return switch", name(2) + name(5), "twomany");
		check("return block", blockValue(4), 9);
		check("return try", tryValue(true) + tryValue(false) * 10, 9);
		check("return in branch", returnInBranch(true) + returnInBranch(false) * 100, 1201);
		check("return in try", returnInTry(true) + returnInTry(false) * 10, 43);

		var sum = 0;
		for (i in 0...10) {
			final v = if (i == 2) continue else if (i == 5) break else i;
			sum += v;
		}
		check("break, continue", sum, 0 + 1 + 3 + 4);

		Sys.println("Done");
	}
}
