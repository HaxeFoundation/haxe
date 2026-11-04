class Main {
	static function main() {}
}

abstract Dummy(Int) {
	macro public function new(v:Int) {
		this = v;
	}
}