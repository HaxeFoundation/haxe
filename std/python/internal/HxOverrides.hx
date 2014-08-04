package python.internal;

import python.Syntax;

import python.Syntax.pythonCode in py;

@:keep
@:native("HxOverrides")
@:access(python.internal.ArrayImpl)
@:access(python.Boot)
class HxOverrides {

	// this two cases iterator and shift are like all methods in String and Array and are already handled in Reflect
	// we need to modify the transformer to call Reflect directly

	static public function iterator(x) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).iterator();
		}
		return Syntax.callField(x, "iterator");
	}

	static function eq( a:Dynamic, b:Dynamic ) : Bool {
		if (Boot.isArray(a) || Boot.isArray(b)) {
			return Syntax.pythonCode('a is b');
		}
		return Syntax.binop(a, "==", b);
	}

	static function stringOrNull (s:String):String {
		return if (s == null) "null" else s;
	}

	static public function shift(x) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).shift();
		}
		return Syntax.callField(x, "shift");
	}

	static public function pop(x) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).pop();
		}
		return Syntax.callField(x, "pop");
	}

	static public function push(x, e) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).push(e);
		}
		return Syntax.callField(x, "push", e);
	}

	static public function join(x, sep) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).join(sep);
		}
		return Syntax.callField(x, "join", sep);
	}

	static public function filter(x, f) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).filter(f);
		}
		return Syntax.callField(x, "filter", f);
	}

	static public function map(x, f) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).map(f);
		}
		return Syntax.callField(x, "map", f);
	}

	static public function toUpperCase(x) {
		if (Boot.isString(x)) {
			return (x:String).toUpperCase();
		}
		return Syntax.callField(x, "toUpperCase");
	}

	static public function toLowerCase(x) {
		if (Boot.isString(x)) {
			return (x:String).toLowerCase();
		}
		return Syntax.callField(x, "toLowerCase");
	}

	static public function rshift(val:Int, n:Int) {
		return Syntax.binop(Syntax.binop(val, "%", Syntax.pythonCode("0x100000000")), ">>", n);
	}

	static public function modf(a:Float, b:Float) {
		return Syntax.pythonCode("float('nan') if (b == 0.0) else a % b if a > 0 else -(-a % b)");
	}

	static public function arrayGet<T>(a:Dynamic, i:Int):Dynamic {
		if (Boot.isArray(a)) {
			return ArrayImpl._get(a, i);
		} else {
			return Syntax.arrayAccess(a, i);
		}
	}

	static public function arraySet(a:Dynamic, i:Int, v:Dynamic) {
		if (Boot.isArray(a)) {
			return ArrayImpl._set(a, i, v);
		} else {
			Syntax.assign(Syntax.arrayAccess(a, i), v);
			return v;
		}
	}

}