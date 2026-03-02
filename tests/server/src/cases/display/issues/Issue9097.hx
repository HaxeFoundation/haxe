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
		Assert.isTrue(locs.exists(l -> l.range == range(2, 3)));
	}
}
