class Main {
	static function main() {}

	@:overload static function conflict(fn:(Int)->String) {}
	@:overload static function conflict(fn:(Bool)->String) {}

	@:overload static function same(fn:(Int)->String) {}
	@:overload static function same(fn:(Int)->String) {}
}