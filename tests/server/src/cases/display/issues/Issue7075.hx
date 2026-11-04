package cases.display.issues;

class Issue7075 extends DisplayTestCase {
	/**
		import hax{-1-}

		class Main {
			static function main() {}
		}
	**/
	function test1(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
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
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.isTrue(result.items.exists(item -> switch item.kind {
			case ClassField: (item.args : Dynamic).field.name == "CallStack";
			case Type: (item.args : Dynamic).path.typeName == "CallStack";
			case Module: (item.args : Dynamic).path.moduleName == "CallStack";
			case _: false;
		}));
	}
}
