package cases.display.issues;

class Issue7761 extends DisplayTestCase {
	/**
		class Main {
			public static function main() {
				Boolean.{-1-}
			}
		}

		@:forwardStatics
		abstract Boolean(BooleanClass) {}

		extern class BooleanClass {
			static function foo():Int;
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "foo";
			case _: false;
		});
	}
}
