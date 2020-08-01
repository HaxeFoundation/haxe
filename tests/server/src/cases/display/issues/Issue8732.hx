package cases.display.issues;

import haxe.display.Protocol;

class Issue8732 extends DisplayTestCase {
	/**
		class Main { static function main() { var ident = "foo"; {-1-}i{-2-}dent.{-3-} } }
	**/
	function test(_) {
		runHaxeJson([], Methods.Initialize, {maxCompletionItems: 50});
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: true});
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(3), wasAutoTriggered: true});
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: true});
		var result = parseCompletion();
		assertHasNoCompletion(result, item -> switch (item.kind) {
			case ClassField: item.args.field.name == "charAt";
			case _: false;
		});
	}
}