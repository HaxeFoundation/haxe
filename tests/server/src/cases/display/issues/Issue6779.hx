package cases.display.issues;

class Issue6779 extends DisplayTestCase {
	/**
		class Some {
			static function f():Void {}
		}

		class Main {
			static function main() {
				@:privateAccess Some.{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "f";
			case _: false;
		});
	}
}
