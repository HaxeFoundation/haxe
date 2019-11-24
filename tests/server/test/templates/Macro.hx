class Macro {
	static var x = 1;

	macro static public function test() {
		Sys.println(x++);
		return macro null;
	}
}
