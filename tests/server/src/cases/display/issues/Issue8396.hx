package cases.display.issues;

class Issue8396 extends DisplayTestCase {
	/**
		enum abstract Test(String) {
			var Foo;
			var Bar;

			function foobar() {
				{-1-}
			}
		}
	**/
	function test(_) {
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		assertHasCompletion(result, item -> switch item.kind {
			case ClassField: item.args.field.name == "Bar" && item.args.field.scope == Member;
			case _: false;
		});
		assertHasNoCompletion(result, item -> switch item.kind {
			case EnumAbstractField: item.args.field.name == "Bar";
			case _: false;
		});
	}
}
