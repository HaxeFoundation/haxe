class Main {
	static function main() {
		new Base();
	}
}

@:build(Macro.build())
class Base {
	public function new() {}
}
