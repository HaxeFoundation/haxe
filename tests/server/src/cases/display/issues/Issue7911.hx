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
		Assert.isTrue(result.result.items.exists(item -> {
			var args:Dynamic = item.args;
			return (item.kind == (cast "Type" : Dynamic) && args.path != null && args.path.typeName == "Test")
				|| (item.kind == (cast "Module" : Dynamic) && args.path != null && args.path.moduleName == "Test");
		}));
	}
}
