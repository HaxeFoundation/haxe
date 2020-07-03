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
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion();
		Assert.equals("obj", result.result.mode.args.item.args.name);
	}
}