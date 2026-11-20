class Main1 {
	static function main() {
		new A();
	}
}

@:build(Macro.build1()) class A {
	public function new() {}
}
