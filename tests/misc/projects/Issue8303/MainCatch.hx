class MainCatch {
	public static function main():Void {
		test();
	}

	static function test() {
		function log() {
			log();
			//prevent tail recursion elimination
			return 0;
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