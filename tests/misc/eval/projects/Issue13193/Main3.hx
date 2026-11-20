class Main3 {
	static function main() {
		new A();
	}
}

@:build(Macro3.build()) class A {
	public function new() {}
}
