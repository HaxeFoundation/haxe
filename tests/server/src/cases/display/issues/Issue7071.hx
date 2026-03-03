package cases.display.issues;

class Issue7071 extends DisplayTestCase {
	/**
		enum Foo { Bar; }

		class Main {
			public static function main() {
				var bar = Bar;
				bar == {-1-};
				if (bar == {-2-})
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "bar");

		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(2), wasAutoTriggered: false});
		result = parseCompletion();
		Assert.isTrue(result.result.items.length > 0);
		assertHasCompletion(result, item -> item.kind == Local && item.args.name == "bar");
	}
}
