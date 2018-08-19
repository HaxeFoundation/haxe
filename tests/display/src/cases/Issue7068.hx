package cases;

class Issue7068 extends DisplayTestCase {
	/**
	enum Foo { Bar; }

	class Main {
		static function main() {
			switch ((null:Foo)) {
				case {-1-} if (false):
			}
		}
	}
	**/
	function test() {
		eq("Bar", toplevel(pos(1))[0].name);
	}

	/**
	enum Foo { Bar; }

	class Main {
		static function main() {
			switch ((null:Foo)) {
				case Bar | {-1-} if (false):
			}
		}
	}
	**/
	function test2() {
		eq("Bar", toplevel(pos(1))[0].name);
	}
}