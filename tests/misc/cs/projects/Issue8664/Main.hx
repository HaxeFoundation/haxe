import cs.NativeArray;

class Main {
	public static function main() {
		new Arr<String>().map(function(v) return Std.parseInt(v));
	}
}

class Arr<T> {
	var __a:NativeArray<T>;

	public function new() {}

	public inline function map<S>(f:T->S) {
		new Arr<S>().__unsafe_set(f(__unsafe_get()));
	}

	inline function __unsafe_get():T {
		return __a[0];
	}

	inline function __unsafe_set(val:T):T {
		return __a[0] = val;
	}
}