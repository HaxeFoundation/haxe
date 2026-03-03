package cases.display.issues;

class Issue7092 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				new {-1-}
			}
		}

		private class PrivateClass {
			public function new() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateClass";
			case _: false;
		});
	}
}
