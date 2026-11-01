package cases.display.issues;

class Issue8046 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				for(k => v in ['hello' => {field:true}]) {
					k.{-1-}
					v.{-2-}
				}
			}
		}
	**/
	function testKeyValue(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});

		result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "field" && item.args.field.type.args.path.typeName == "Bool";
			case _: false;
		});
	}
}
