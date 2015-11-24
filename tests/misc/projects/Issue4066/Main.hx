class Main {
	static function main() {
		var a = new A();
		function print(s) {
			Sys.stderr().writeString(s + "\n");
		}
		print(a.doSomething());
		print(a.doSomethingElse());
	}
}