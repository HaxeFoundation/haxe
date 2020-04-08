class Main {
	static function main() {
		test();
	}

	macro static public function test() {
		haxe.macro.Context.getType("");
		return macro {};
	}
}