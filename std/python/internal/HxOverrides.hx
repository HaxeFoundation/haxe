package python.internal;

import python.Syntax;

import python.Syntax.untypedPython in py;

@:keep
@:native("HxOverrides")
class HxOverrides {
	static public function iterator(x) {
		if (Std.is(x, Array)) {
			return Syntax.untypedPython(" _hx_c.python_internal_ArrayImpl.iterator(x)");
		} else {
			return Syntax.untypedPython("x.iterator()");
		}
	}

	static public function shift(x) {
		if (Std.is(x, Array)) {
			return Syntax.untypedPython(" _hx_c.python_internal_ArrayImpl.shift(x)");
		} else {
			return Syntax.untypedPython("x.shift()");
		}
	}

	static public function filter(x, f) {
		if (Std.is(x, Array)) {
			return (x:Array<Dynamic>).filter(f);
		} else {
			return Syntax.untypedPython("x.filter(f)");
		}
	}

	static public function map(x, f) {
		if (Std.is(x, Array)) {
			return Syntax.untypedPython(" _hx_c.python_internal_ArrayImpl.map(x, f)");
		} else {
			return Syntax.untypedPython("x.map(f)");
		}
	}

	static public function length(x:Dynamic) {
		return if (Std.is(x, Array))
		{
			(x:Array<Dynamic>).length;
		}
		else if (Std.is(x, String))
		{
			(x:String).length;
		}
		else
		{
			Reflect.field(x, "length");
		}
	}

	static public function hx_rshift(val, n) {
		return Syntax.binop(Syntax.binop(val, "%", Syntax.untypedPython("0x100000000")), ">>", n);

	}

	static public function hx_modf(a, b) {
		return Syntax.untypedPython("float('nan') if (b == 0.0) else a % b if a > 0 else -(-a % b)");
	}

	static public function hx_array_get<T>(a:Array<T>, i:Int):T {
		return if (i < a.length && i > -1) Syntax.arrayAccess(a, i) else null;
	}

	static public function hx_array_set<X>(a:Array<X>, i:Int, v:X) {
		var l = a.length;
		while (l < i) {
			a.push(null);
			l+=1;
		}
		if (l == i) {
			a.push(v);
		} else {
			Syntax.assign(Syntax.arrayAccess(a, i), v);
		}
		return v;

	}

	static public function hx_toUpperCase(x) {
		if (Std.is(x, String)) {
			return (x:String).toUpperCase();
		} else {
			return x.toUpperCase();
		}
	}

	static public function hx_toLowerCase(x) {
		if (Std.is(x, String)) {
			return (x:String).toLowerCase();
		} else {
			return x.toLowerCase();
		}
	}
}