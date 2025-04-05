package cases.display.issues;

class Issue9446 extends DisplayTestCase {
	/**
		function fu{-1-}nc() {}

		function main() {
			{-2-}func{-3-}();
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.FindReferences, {file: file, offset: offset(1)});
		var result = parseGotoDefinitionLocations();
		Assert.same([range(2, 3)], result.map(l -> l.range));
	}
}
