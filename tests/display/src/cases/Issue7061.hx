package cases;

class Issue7061 extends DisplayTestCase {
	/**
		abstract Either<A, B>(EitherImpl<A, B>) {
			function new(value) this = value;
			@:from static function fromA<A,B>(value:A) return new Either(a(value));
			@:from static function fromB<A,B>(value:B) return new Either(b(value));
		}

		enum EitherImpl<A, B> {
			a(v:A);
			b(v:B);
		}

		class Main {
			static function main() {}
			function new() f{-6-}oo(b{-1-}ar);
			function notNew() foo(b{-7-}ar2);
			function {-2-}foo{-3-}<T>(value:Either<()->T,()->Void>) {}
			function {-4-}bar{-5-}() {}
			function bar2() return 1;
		}
	**/
	function test() {
		eq(range(4, 5), position(pos(1)));
		eq(range(2, 3), position(pos(6)));
		eq("() -> Void", type(pos(1)));
		eq("() -> Int", type(pos(7)));
	}
}
