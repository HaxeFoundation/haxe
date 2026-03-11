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
		var result = runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		Assert.isTrue(result != null);
	}
}
