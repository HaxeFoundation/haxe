class Main {
	static function main() {}
}

abstract Abstr(String) {
	@:to public static function staticTo():String
		return '';

	@:to public function instanceTo(a:Int):String
		return '';
}