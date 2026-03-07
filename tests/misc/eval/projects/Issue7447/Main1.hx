class Main1 {
	static function main() {

	}
}

abstract Abstr(Int) {
	public inline function new() {
		trace(() -> this);
		this = 0;
	}
}