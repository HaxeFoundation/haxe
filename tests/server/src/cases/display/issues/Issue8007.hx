package cases.display.issues;

class Issue8007 extends DisplayTestCase {
	/**
		class Main { static function main() {
			var i:Null<{-1-}>
		} }
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Array";
			case _: false;
		});
		assertHasCompletion(result, item -> item.kind == Package && item.args.path.pack[0] == "haxe");
	}

	/**
		class Main { static function main() {
			var i:Null<{-1-}
		} }
	**/
	function test2(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Array";
			case _: false;
		});
		assertHasCompletion(result, item -> item.kind == Package && item.args.path.pack[0] == "haxe");
	}
}
