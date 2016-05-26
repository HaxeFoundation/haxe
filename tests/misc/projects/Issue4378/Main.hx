class Main implements I {

	function new() { }

	public function test(s:String) {
		trace(s);
	}

	static function main() {
		var m:I = new Main();
		m.test(1);
	}
}

interface I {
	function test(s:Dynamic):Void;
}