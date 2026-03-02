package cases.display.issues;

class Issue7911 extends DisplayTestCase {
	/**
		import issue7911.{-1-}
	**/
	function test(_) {
		vfs.putContent("issue7911/Test.hx", "package issue7911;\n");
		vfs.putContent("issue7911/import.hx", "package issue7911;\n");
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		var result = parseCompletion();
		Assert.equals(1, result.result.items.length);
		assertHasCompletion(result, item -> switch item.kind {
			case Type: item.args.path.typeName == "Test";
			case Module: item.args.path.moduleName == "Test";
			case _: false;
		});
	}
}
