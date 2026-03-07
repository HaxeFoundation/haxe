class Main {
	static function main() {
		trace(Abs.use(fn)(101));
	}

	inline static function fn(v:Int):Int
		return v;
}

@:callable abstract Abs<T>(T->Int) from T->Int {
	inline public static function use<T>(fn:T->Int):Abs<T> {
		return fn;
	}
}

