package cases.display.issues;

class Issue8420 extends DisplayTestCase {
	/**
		class A {
			final x:Int;

			function f() {
				this.{-1-}
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "x" && item.args.field.type.args.path.typeName == "Int";
			case _: false;
		});
	}
}
