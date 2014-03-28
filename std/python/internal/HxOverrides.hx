package python.internal;

@:keep
@:native("HxOverrides")
class HxOverrides {
	static public function iterator(x) {
		if (Std.is(x, Array)) {
			return untyped __python__(" _hx_c.python_internal_ArrayImpl.iterator(x)");
		} else {
			return untyped __python__("x.iterator()");
		}
	}

	static public function shift(x) {
		if (Std.is(x, Array)) {
			return untyped __python__(" _hx_c.python_internal_ArrayImpl.shift(x)");
		} else {
			return untyped __python__("x.shift()");
		}
	}

	static public function filter(x, f) {
		if (Std.is(x, Array)) {
			return untyped __python__(" _hx_c.python_internal_ArrayImpl.filter(x, f)");
		} else {
			return untyped __python__("x.filter(f)");
		}
	}

	static public function map(x, f) {
		if (Std.is(x, Array)) {
			return untyped __python__(" _hx_c.python_internal_ArrayImpl.map(x, f)");
		} else {
			return untyped __python__("x.map(f)");
		}
	}

	static public function length(x) {
		if (Std.is(x, Array) || Std.is(x, String)) {
			return untyped __python__(" _hx_builtin.len(x)");
		} else {
			return untyped __python__("x.length");
		}
	}

	static public function hx_rshift(val, n) {
		return untyped __python__("(val % 0x100000000) >> n");
	}

	static public function hx_modf(a, b) {
		return untyped __python__("float('nan') if (b == 0.0) else a % b if a > 0 else -(-a % b)");
	}

	static public function hx_array_get(a, i) {
		return untyped __python__("a[i] if (i < len(a) and i > -1) else None");
	}

	static public function hx_array_set(a, i, v) {
		untyped __python__("
			l = len(a)
			while l < i:
				a.append(None)
				l+=1
			if l == i:
				a.append(v)
			else:
				a[i] = v
			return v");
	}

	static public function hx_toUpperCase(x) {
		if (Std.is(x, String)) {
			return x.upper();
		} else {
			return x.toUpperCase();
		}
	}
}