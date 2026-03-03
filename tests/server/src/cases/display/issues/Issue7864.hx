package cases.display.issues;

class Issue7864 extends DisplayTestCase {
	/**
		@{-1-}
		class Main {
			static function main() {
			}
		}

		@{-2-}
		class Test {}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Metadata && item.args.name == "@:enum");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Metadata && item.args.name == "@:enum");
	}
}
