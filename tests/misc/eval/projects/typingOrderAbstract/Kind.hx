abstract Kind(Int) from Int to Int {
	public inline function new(v:Int) {
		this = cached(v);
	}
	static function cached(v:Int):Kind {
		var k:Kind = v;
		return k;
	}
	@:from public static inline function fromString(s:String) {
		return new Kind(Std.parseInt(s));
	}
	public function toInt() return this;
}
