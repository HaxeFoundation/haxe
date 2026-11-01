class Main {
	public static function level2():Void {
		level3();
	}

	public static function level3():Void {
		throw "!";
	}

	static public function main() {
		level2();
	}
}