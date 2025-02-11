package cases.issues;

class Issue12001 extends TestCase {
	function testDefineType(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.defineType()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		Assert.isFalse(0 == errorMessages.length);
		assertErrorMessage("Cannot redefine module Foo");
	}

	function testDefineModule(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.defineModule()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		Assert.isFalse(0 == errorMessages.length);
		assertErrorMessage("Cannot redefine module Bar");
	}
}
