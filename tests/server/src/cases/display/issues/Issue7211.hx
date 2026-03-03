package cases.display.issues;

class Issue7211 extends DisplayTestCase {
	/**
		class Main { static function main() {
			var s:{f:String} = null;
			switch s.{-1-}
		} }
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "f" && item.args.field.type.args.path.typeName == "String";
			case _: false;
		});
	}
}
