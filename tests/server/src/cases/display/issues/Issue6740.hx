package cases.display.issues;

class Issue6740 extends DisplayTestCase {
	/**
		class A {
			function new(v:Array<String>) {
			}
		}

		class Main extends A {

			function new(v) {
				super(v);
				v.{-1-}
			}

			static function main() {
				new Main();
			}

		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "concat";
			case _: false;
		});
	}
}
