package cases.display.issues;

class Issue7577 extends DisplayTestCase {
	/**

		@:forward abstract X(String) {}

		class Main {
			static function test(x:X) {
				x.{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "charCodeAt";
			case _: false;
		});
	}
}
