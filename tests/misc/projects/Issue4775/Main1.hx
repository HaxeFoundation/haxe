@:generic class Contain<T:String> {
	public function new(t:T) { }
}

class Main1 {
	static function main() {
		new Contain(new Main1());
	}

	function new() { }
}