class MainCatch {
	public static function main():Void {
		test();
	}

	static function test() {
		function log() {
			log();
		}
		try {
			log();
		} catch(s:String) {
			if(s != 'Stack overflow') {
				Sys.exit(1);
			}
		}
		return macro {};
	}
}