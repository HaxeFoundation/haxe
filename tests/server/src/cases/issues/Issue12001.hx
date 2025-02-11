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

	function testDefineType1(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main.hx"));
		var args = ["-main", "Main", "--macro", "Macro.defineType()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		Assert.isFalse(hasErrorMessage('HxbData.HxbFailure("Could not read static field test on Foo while hxbing Main")'));
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

	function testDefineModule1(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main1.hx"));
		var args = ["-main", "Main", "--macro", "Macro.defineModule()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		Assert.isFalse(0 == errorMessages.length);
		Assert.isFalse(hasErrorMessage('HxbData.HxbFailure("Could not read static field test on Bar while hxbing Main")'));
		assertErrorMessage("Cannot redefine module Bar");
	}

	function testAfterTyping(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.hook()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		Assert.isFalse(0 == errorMessages.length);
		assertErrorMessage("Cannot redefine module Baz");
	}
}
