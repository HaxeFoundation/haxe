class C {
	public final x:Int;
	public function new() {
		x = 10;
	}
}

class Main {
	function new() {
		new C().x = 10;
	}

	static function main() {
	}
}