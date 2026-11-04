package cases.display.issues;

class Issue7172 extends DisplayTestCase {
	/**
		class Main {
			static function main() {}

			function foo() {
				this.{-1-}


				var x:Int;
			}
		}
	**/
	function testNo(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasNoCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "x" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}

	/**
		class Main {
			static function main() {}

			function foo() {
				this.{-1-}

			}

				var x:Int;
		}
	**/
	function testYes(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "x" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
