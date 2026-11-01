package cases.display.issues;

class Issue5988 extends DisplayTestCase {
	/**
		class Main {
			static function f():Int;

			static function main() {
				{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Array";
			case _: false;
		});
	}
}
