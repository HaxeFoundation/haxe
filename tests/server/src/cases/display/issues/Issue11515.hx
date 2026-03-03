package cases.display.issues;

class Issue11515 extends DisplayTestCase {
	/**
		import haxe.ds.Option;

		class Main {
			static function main () {
				Option.{-1-}
			}
		}
	**/
	function testImport(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case EnumField: item.args.field.name == "None";
			case _: false;
		});
	}

	/**
		class Main {
			static function main () {
				haxe.ds.Option.{-1-}
			}
		}
	**/
	function testFully(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case EnumField: item.args.field.name == "None";
			case _: false;
		});
	}
}
