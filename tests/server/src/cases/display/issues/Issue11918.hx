package cases.display.issues;

class Issue11918 extends DisplayTestCase {
	/**
		function main() {
			final foo = get();
			// foo is mono, no completion for String methods
			foo.{-1-}
		}

		function get<T:String>():T {
			return cast "";
		}
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "length" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
