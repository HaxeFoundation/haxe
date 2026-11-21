class Main4 {
	static function main() {
		new A();
	}
}

@:build(Macro4.build()) class A {
	public function new() {}
}
