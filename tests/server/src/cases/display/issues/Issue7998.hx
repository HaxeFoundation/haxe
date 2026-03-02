package cases.display.issues;

class Issue7998 extends DisplayTestCase {
	/**
		class Main { static function main() {
			var subject:{
				var iterator:Int;
				var keyValueIterator:Float;
			};
			subject.{-1-}
		} }
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "iterator" && item.args.field.kind.kind == FVar;
			case _: false;
		});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "keyValueIterator" && item.args.field.kind.kind == FVar;
			case _: false;
		});
	}
}
