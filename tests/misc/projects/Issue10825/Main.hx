class A {
	public function new() {}

	public final function run() {
		trace("running");
	}
}

class Main {
	static function main() {
		final a:A = null;
		a.run();
	}
}
