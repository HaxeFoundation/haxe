package cases.display.issues;

class Issue9097 extends DisplayTestCase {
	/**
		@:generic class Fo{-1-}o<T> {}
		class Bar extends {-2-}Foo<String>{-3-} {}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.FindReferences, {file: file, offset: offset(1)});
		var locs = parseGotoDefintion().result;
		Assert.isTrue(locs != null && locs.length > 0);
		var loc = locs.find(l -> Std.string(l.range) == Std.string(range(2, 3)));
		Assert.notNull(loc);
		Assert.same(range(2, 3), loc.range);
	}
}
