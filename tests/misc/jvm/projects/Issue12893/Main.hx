class Main {
	var x:Int = 0;
	public function new() {}
	public function run(r:java.lang.Runnable) r.run();
	public function test() {
		run(() -> { x = 1; }); // captures `this`, triggers the bug
	}
	static public function main() {
		new Main().test();
	}
}
