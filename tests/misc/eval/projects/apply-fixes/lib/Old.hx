class Old {
	public var n:Int;

	public function new(n:Int) {
		this.n = n;
	}

	@:deprecated("Use add instead")
	@:deprecated.replace(this.add(x * 2))
	public function twice(x:Int) {
		return add(x * 2);
	}

	public function add(x:Int) {
		return n + x;
	}

	@:deprecated("Use make instead")
	@:deprecated.replace(Old.make(a, b))
	public static function create(a:Int, ?b:Int) {
		return make(a, b);
	}

	public static function make(a:Int, ?b:Int) {
		return new Old(a);
	}

	public static function fromLib() {
		return Std.is(1, Int);
	}
}
