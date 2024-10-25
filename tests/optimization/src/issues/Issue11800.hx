package issues;

class Issue11800 {
	@:js('
		++issues_Issue11800.test_a;
		++issues_Issue11800.test_b;
		++issues_Issue11800.test_a;
		++issues_Issue11800.test_b;
	')
	static function test() {
		static var a = 0;

		for (i in 0...3) {
			switch i {
				case n if (n < 2):
					use(++a);
					static var b = 0;
					use(++b);
				case _:
			}
		}
	}

	static function use(v:Int) {}
}