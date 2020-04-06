class Main {
	static function main() {}
}

abstract Color(Int) from Int to Int  {
	var argb(never,set):Int;

	inline function set_argb(v) return this = v;

	public function copyFrom(c:Color) {
		argb = c;
	}
}