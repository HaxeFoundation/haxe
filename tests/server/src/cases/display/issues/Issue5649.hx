package cases.display.issues;

class Issue5649 extends DisplayTestCase {
	/**
		class Main {
			public static function main():{-1-} {-2-} {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Array";
			case _: false;
		});
	}
}
