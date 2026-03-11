package cases.display.issues;

class Issue7937 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}
		}

		typedef FoldingRangeServerCapabilities = {
			var foldingRa{-1-}ngeProvider:FoldingRangeProviderOptions & {};
		}

		typedef FoldingRangeProviderOptions = {}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		Assert.isTrue(result.result != null);
	}
}
