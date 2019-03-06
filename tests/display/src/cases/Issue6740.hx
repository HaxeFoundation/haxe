package cases;

class Issue6740 extends DisplayTestCase {
	/**
		class A {
			function new(v:Array<String>) {
			}
		}

		class Main extends A {

			function new(v) {
				super(v);
				v.{-1-}
			}

			static function main() {
				new Main();
			}

		}
	**/
	function test() {
		eq(true, hasField(fields(pos(1)), "concat", "(a : Array<String>) -> Array<String>"));
	}
}
