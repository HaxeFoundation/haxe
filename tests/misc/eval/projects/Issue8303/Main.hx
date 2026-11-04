class Main {
	public static function main():Void {
		test();
	}

	static function test() {
		function log() {
			log();
		}
		log();
		return macro {};
	}
}