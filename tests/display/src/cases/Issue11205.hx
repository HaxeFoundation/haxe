package cases;

class Issue11205 extends DisplayTestCase {
	/**
		typedef Foo = {
			var {-1-}bar{-2-}:{
				var {-3-}value{-4-}:Int;
			};
		}
		function main() {
			final foo:Foo = cast null;
			foo?.b{-5-}ar?.v{-6-}alue;
		}
	**/
	function test() {
		eq(range(1, 2), position(pos(5)));
		eq("{ value : Int }", type(pos(5)));

		eq(range(3, 4), position(pos(6)));
		eq("Null<Int>", type(pos(6)));
	}
}
