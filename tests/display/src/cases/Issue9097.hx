package cases;

class Issue9097 extends DisplayTestCase {
	/**
		@:generic class Fo{-1-}o<T> {}
		class Bar extends {-2-}Foo<String>{-3-} {}
	**/
	function test() {
		arrayEq([range(2, 3)], ctx.usage(pos(1)));
	}
}
