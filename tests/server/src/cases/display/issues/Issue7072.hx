package cases.display.issues;

class Issue7072 extends DisplayTestCase {
	/**
		typedef Struct = {
			var foo:Int;
			var bar:Int;
			var foobar:Int;
		}

		class Main {
			public static function main() {
				var s:Struct = {
					{-1-}
				}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.pass(); // TODO: test field ordering
	}
}
