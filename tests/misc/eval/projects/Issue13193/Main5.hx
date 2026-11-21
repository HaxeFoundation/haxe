class Main5 {
	static function main() {
		new A();
	}
}

@:build(Macro5.build()) class A {
	public function new() {}
}
