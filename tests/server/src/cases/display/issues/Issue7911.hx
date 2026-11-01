package cases.display.issues;

class Issue7911 extends DisplayTestCase {
	/**
		import issue7911.{-1-}
	**/
	function test(_) {
		vfs.putContent("issue7911/Test.hx", "package issue7911;\n");
		vfs.putContent("issue7911/import.hx", "package issue7911;\n");
		var result = runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(1), wasAutoTriggered: false});
		Assert.equals(1, result.items.length);
		final item = result.items[0];
		switch item.kind {
			case Type:
				Assert.equals("Test", item.args.path.typeName);
			case Module:
				Assert.equals("Test", item.args.path.moduleName);
			case _:
				Assert.fail('Unexpected item kind: ${item.kind}');
		}
	}
}
