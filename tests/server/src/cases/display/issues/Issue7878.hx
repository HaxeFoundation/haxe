package cases.display.issues;

class Issue7878 extends DisplayTestCase {
	/**
		class Main {
		public static function main() {
			var f:Array<SomethingUnk{-1-}nown>;
		}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
		var result = parseHover();
		// Hover on unknown type - should return null or an error
		Assert.isTrue(result.result == null || result.error != null);
	}
}
