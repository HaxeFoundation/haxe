class Main2 {
	static function main() {
		new A();
	}
}

@:build(Macro.build2()) class A {
	public function new() {}
}
