package cases.display.issues;

class Issue3907 extends DisplayTestCase {
	/**
		class Main {
		    static function doStuff(options:{a:Float, b:String}) {}

		    static function main () {
		        doStuff({
		            {-1-}
		        });
		    }
		}
	**/
	function testAnonStructCompletion(_) {
		var items = fields(1);
		Assert.isTrue(hasField(items, "a"));
		Assert.isTrue(hasField(items, "b"));
	}
}
