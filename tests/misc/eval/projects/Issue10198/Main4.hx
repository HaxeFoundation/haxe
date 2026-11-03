class Main4 {
	static function main() {
		var n = null;
		fn1(1, n);
		n = 1.2;
	}

	static function fn1<T, R:T>(r:R, t:Null<T>) {}
}