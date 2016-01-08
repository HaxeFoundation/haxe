@:generic class Contain<T:String> {
	public function new(t:T) { }
}

class Main {
	static function main() {
		new Contain(new Main());
	}

	function new() { }
}