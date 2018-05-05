class Main {
    static function main() {
		var i = getInt();
		call(i++, i > 0 ? { call(0, 0); 1; } : 1);
    }

	static function getInt() { return 0; }

	static function call(a:Int, b:Int) { Sys.stderr().writeString(a + " " + b + "\n"); }
}