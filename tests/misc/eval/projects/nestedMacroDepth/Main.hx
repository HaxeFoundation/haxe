class Main {
	static function main() {
		new A();
	}
}

// each build macro recurses 600 levels deep (under the default limit of 1000), A types B in the middle of its own
@:build(Nest.build(600, "B")) class A { public function new() {} }
@:build(Nest.build(600)) class B { public function new() {} }
