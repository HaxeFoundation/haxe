package cases.display.issues;

class Issue7627 extends DisplayTestCase {
	/**

		import haxe.ds.Option;

		class Main {
			public static function main() {
				var option = Some(1);
				option.match(None);
				option.{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "match";
			case _: false;
		});
	}
}
