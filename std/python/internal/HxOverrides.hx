package python.internal;

import python.Syntax;

import python.Syntax.pythonCode in py;

@:keep
@:native("HxOverrides")
@:access(python.internal.ArrayImpl)
class HxOverrides {

	// this two cases iterator and shift are like all methods in String and Array and are already handled in Reflect
	// we need to modify the transformer to call Reflect directly

	static public function iterator(x) {
		if (Std.is(x, Array)) {
			return (x:Array<Dynamic>).iterator();
		}
		return Reflect.callMethod(null, Reflect.field(x, "iterator"), []);
	}

	static public function shift(x) {
		return Reflect.callMethod(null, Reflect.field(x, "shift"), []);
	}
	static public function toUpperCase(x) {
		return Reflect.callMethod(null, Reflect.field(x, "toUpperCase"), []);
	}

	static public function toLowerCase(x) {
		return Reflect.callMethod(null, Reflect.field(x, "toLowerCase"), []);
	}

	static public function rshift(val:Int, n:Int) {
		return Syntax.binop(Syntax.binop(val, "%", Syntax.pythonCode("0x100000000")), ">>", n);
	}

	static public function modf(a:Float, b:Float) {
		return Syntax.pythonCode("float('nan') if (b == 0.0) else a % b if a > 0 else -(-a % b)");
	}

	static public function arrayGet<T>(a:Dynamic, i:Int):Dynamic {
		if (Std.is(a, Array)) {
			return ArrayImpl.__get(a, i);
		} else {
			return Syntax.arrayAccess(a, i);
		}
	}

	static public function arraySet(a:Dynamic, i:Int, v:Dynamic) {
		if (Std.is(a, Array)) {
			return ArrayImpl.__set(a, i, v);
		} else {
			Syntax.assign(Syntax.arrayAccess(a, i), v);
			return v;
		}
	}

}