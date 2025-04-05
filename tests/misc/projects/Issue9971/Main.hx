class Main {
	static function main() {
		var a:Array<{ foo: Int, bar: String }> = [for (i in 0...1+Std.random(9)) {}];
	}
}