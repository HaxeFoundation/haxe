class Main {
	static function main() {
		var tests = Macro.getCases("cases");
		utest.UTest.run(tests);
	}
}
