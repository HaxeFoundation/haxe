package cases.display.issues;

class Issue7650 extends DisplayTestCase {
	/**
		class Main { static function main() {
			new MyClasss("".{-1-}
		} }
	**/
	function test1(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}

	/**
		class Main { static function main() {
			new MyClasss<MyClassss>("".{-1-}
		} }
	**/
	function test2(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
