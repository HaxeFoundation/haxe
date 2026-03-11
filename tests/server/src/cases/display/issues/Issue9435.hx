package cases.display.issues;

class Issue9435 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var i:Int;
				foo(i, {-1-})
			}

			static function foo(arg:Int) {}
		}
	**/
	function testCatch_noTypeHint(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "i");
	}
}
