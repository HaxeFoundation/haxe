class Main {
	static function main() {
		var x = Test.test();
		x = new Main();
		x.foo = "wtf";
	}

	function new() {}
}
