package cases.display.issues;

class Issue7281 extends DisplayTestCase {
	/**
		extern class PrivateExternConstructor {
			function new() { }
		}

		class PrivateConstructor {
			function new() { }

			static function test() {
				new {-1-}
			}
		}

		class Ext1 extends PrivateConstructor {
			static function test() {
				new {-2-}
			}
		}

		class Main {
			static function main() {
				new {-3-}
				@:privateAccess new {-4-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateConstructor";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateExternConstructor";
			case _: false;
		});

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateConstructor";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateExternConstructor";
			case _: false;
		});

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(3), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasNoCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateConstructor";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateExternConstructor";
			case _: false;
		});

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(4), wasAutoTriggered: false});
		result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateConstructor";
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "PrivateExternConstructor";
			case _: false;
		});
	}
}
