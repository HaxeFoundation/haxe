typedef S = { f:Int };

class Main {
	static function main() {
		var v = f2(f());
		(v : S);
		$type(v);
	}

	static function f():Dynamic return null;
	static function f2<T>(v:T):Null<T> return v;
}