class Main {
	public static function main():Void
		trace(foo());

	public static function foo():Int {
		true || throw 0;
		0;
	}
}
