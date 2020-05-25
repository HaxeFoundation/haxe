package cases.display.issues;

class Issue7697 extends DisplayTestCase {
	/**
		import data.function.{-1-}

		class Main {
			function main() {}
		}
	**/
	function test(_) {
		vfs.putContent("data/function/Test.hx", getTemplate("issues/Issue7697/data/function/Test.hx"));
		runHaxeJson([], DisplayMethods.Completion, {
			file: file,
			offset: offset(1),
			wasAutoTriggered: true
		});
		var result = parseCompletion().result;
		Assert.equals(1, result.items.length);
		Assert.equals('Test', result.items[0].args.path.moduleName);
	}
}
