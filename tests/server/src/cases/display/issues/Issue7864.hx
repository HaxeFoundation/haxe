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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> item.kind == Metadata && item.args.name == "@:enum");

		result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		assertHasCompletion(result, item -> item.kind == Metadata && item.args.name == "@:enum");
	}
}
