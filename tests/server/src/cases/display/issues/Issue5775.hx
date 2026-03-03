package cases.display.issues;

class Issue5775 extends DisplayTestCase {
	/**
		class Main {
			static function main () {
				var a = [];
				trace('${a.{-1-}}');
			}
		}
	**/
	function testType1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
