class Main {
	public static function main():Void {}
}

abstract Abstr(Int) {
	inline function new(v) {
		this = v;
	}

	public function test() {
		new Abstr(this);
	}
}
