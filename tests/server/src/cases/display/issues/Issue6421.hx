package cases.display.issues;

class Issue6421 extends DisplayTestCase {
	/**
		using Main.Abstract;

		abstract Abstract(Int) {
			public function new(i) this = i;
			public function foo():Void { }
		}

		class Main {
			static function main() {
				0.{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "foo";
			case _: false;
		});
	}
}
