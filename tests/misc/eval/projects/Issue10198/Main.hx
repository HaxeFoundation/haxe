class Main {
	static function main()
		var b:Int = fn(fn('s'));

	static public function fn<T, R:T>(v:R):T
		return null;
}