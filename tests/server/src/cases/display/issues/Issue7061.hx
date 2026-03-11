package cases.display.issues;

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
	function test(_) {
		var locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(1)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(4, 5), locs[0].range);

		locs = runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(6)});
		Assert.isTrue(locs != null && locs.length > 0);
		Assert.same(range(2, 3), locs[0].range);

		Assert.isTrue(runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)}).item.type.kind == (cast "TFun" : Dynamic));
	}
}
