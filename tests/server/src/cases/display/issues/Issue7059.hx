package cases.display.issues;

class Issue7059 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> item.kind == Literal && item.args.name == "trace");
	}
}
