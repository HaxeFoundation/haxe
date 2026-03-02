package cases.display.issues;

import TestCase;

class Issue7878 extends DisplayTestCase {
	/**
		class Main {
		public static function main() {
			var f:Array<SomethingUnk{-1-}nown>;
		}
		}
	**/
	function test(_) {
		try {
			runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(1)});
			Assert.fail();
		} catch (e:TestException) {
			Assert.isTrue(e.message.indexOf("SomethingUnknown") >= 0);
		}
	}
}
