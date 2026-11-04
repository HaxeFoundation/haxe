package cases.display.issues;

class Issue7047 extends DisplayTestCase {
	/**
		class Main {
			var f:()->{-1-}

			static function main() {}
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
