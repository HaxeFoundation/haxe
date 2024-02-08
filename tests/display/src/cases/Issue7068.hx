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
		eq(true, isToplevel(toplevel(pos(1))[0], "Bar"));
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
		eq(true, isToplevel(toplevel(pos(1))[0], "Bar"));
	}
}
