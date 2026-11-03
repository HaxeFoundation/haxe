package cases.display.issues;

class Issue7471 extends DisplayTestCase {
	/**
		class Main { static function main() {
			return "foo" == null ? "foo".{-1-}
		} }
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
