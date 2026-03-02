package cases.display.issues;

class Issue7075 extends DisplayTestCase {
	/**
		import hax{-1-}

		class Main {
			static function main() {}
		}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "CallStack";
			case _: false;
		});
	}

	/**
		import haxe.d{-1-}

		class Main {
			static function main() {}
		}
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "CallStack";
			case Type: item.args.path.typeName == "CallStack";
			case Module: item.args.path.moduleName == "CallStack";
			case _: false;
		});
	}
}
