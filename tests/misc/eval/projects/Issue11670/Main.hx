class Main {
	static function main() {
		test(var foo:String);
	}

	static macro function test(e) {
		switch e {
			// Unrecognized pattern: untyped $__mk_pos__("Test.hx", 145, 150)
			case macro var $name:$ct:
			case _:
		}

		return macro {};
	}
}

