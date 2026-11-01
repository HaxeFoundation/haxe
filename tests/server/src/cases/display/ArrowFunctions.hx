package cases.display;

class ArrowFunctions extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var obj = { foo : 1 };
				var f = () -> obj.{-1-}
				var f = () -> {
					[1].{-2-}
				}
			}
		}
	**/
	function testBodyCompletion1(_) {
		eq(true, hasField(fields(1), "foo", "Int"));
		eq(true, hasField(fields(2), "copy", "() -> Array<Int>"));
	}

	/**
		enum E { EA; EB; EC; }
		class SomeClass {
			static function sf(){
				(e:E) -> e.{-1-}
			}
		}
	**/
	function testBodyCompletion2(_) {
		eq(true, hasField(fields(1), "getName", "() -> String"));
	}

	/**
		class Main {
			static function main() {
				var arr = [1,2,3,4,5];
				arr.map( {-1-}a -> a{-2-} + 1 )
			}
		}
	**/
	function testHover(_) {
		eq("Int", type(1));
		eq("Int", type(2));
	}

	/**
		class Main {
			static function main() {
				x -> { {-1-}
			}
		}
	**/
	function testTopLevel(_) {
		eq(true, hasToplevel(toplevel(1), "local", "x"));
	}
}
