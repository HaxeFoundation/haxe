package cases;

class Issue11918 extends DisplayTestCase {
	/**
		function main() {
			final foo = get();
			// foo is mono, no completion for String methods
			foo.{-1-}
		}

		function get<T:String>():T {
			return cast "";
		}
	**/
	function test1() {
		eq(true, hasField(fields(pos(1)), "length", "Int"));
	}
}
