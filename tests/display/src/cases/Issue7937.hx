package cases;

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
	function test() {
		eq("{ }", type(pos(1)));
	}
}
