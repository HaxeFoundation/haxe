class Main {
	static function main() {
		var v;
		trace(() -> v);
	}
}

abstract Abstr(Int) {
	public inline function new() {
		trace(() -> this);
		this = 0;
	}
}