class Main {
	static public function main() {
		var runner = new haxe.unit.TestRunner();
		runner.add(new io.TestFileInput());
		runner.run();
	}
}