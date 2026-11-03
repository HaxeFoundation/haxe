package cases.display.issues;

class Issue6005 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var foo:Struct = {
					{-1-}
				}
			}
		}

		typedef Struct = {
			integer:Int
		}
	**/
	function testTypedefStructCompletion(_) {
		var items = fields(1);
		Assert.isTrue(hasField(items, "integer"));
	}
}
