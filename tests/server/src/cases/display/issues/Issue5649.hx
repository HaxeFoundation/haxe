package cases.display.issues;

class Issue5649 extends DisplayTestCase {
	/**
		class Main {
			public static function main():{-1-} {-2-} {}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Array";
			case _: false;
		});
	}
}
