package cases;

class ArrowFunctions extends DisplayTestCase {
	/**
		var obj = { foo : 1 };
		var f = () -> obj.{-1-}
		var f = () -> {
			[1].{-2-}
		}
	**/
	@:funcCode function testBodyCompletion1() {
		eq(true, hasField(fields(pos(1)), "foo", "Int"));
		eq(true, hasField(fields(pos(2)), "copy", "() -> Array<Int>"));
	}

	/**
		enum E { EA; EB; EC; }
		class SomeClass {
			static function sf(){
				(e:E) -> e.{-1-}
			}
		}
	**/
	function testBodyCompletion2() {
		eq(true, hasField(fields(pos(1)), "getName", "() -> String"));
	}

	/**
		var arr = [1,2,3,4,5];
		arr.map( {-1-}a -> a{-2-} + 1 )
	**/
	@:funcCode function testHover() {
		eq("Int", type(pos(1)));
		eq("Int", type(pos(2)));
	}

	/**
		x -> { {-1-}
	**/
	@:funcCode function testTopLevel() {
		eq(true, hasToplevel(toplevel(pos(1)), "local", "x"));
	}
}
