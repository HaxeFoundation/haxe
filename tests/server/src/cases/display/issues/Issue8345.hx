package cases.display.issues;

class Issue8345 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				var i:Test2 = null;
				i.{-1-};
			}
		}

		interface Test {
			var foo:Int;
		}

		interface Test2 extends Test {}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "foo" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
