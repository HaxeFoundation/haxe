package cases.display.issues;

class Issue7317 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				var obj = {};
				obj.{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		Assert.equals("obj", result.mode.args.item.args.name);
	}
}