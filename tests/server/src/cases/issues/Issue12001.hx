package cases.issues;

import utest.Async;

class Issue12001 extends TestCase {
	function testDefineType(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.defineType()"];
		runHaxe(args);
		assertSuccess();

		// Nothing is loading Foo, so no redefinition issue
		runHaxe(args);
		assertSuccess();
	}

	function testDefineType1(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main.hx"));
		var args = ["-main", "Main", "--macro", "Macro.defineType()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		assertSuccess();
		// TODO: change invalidation reason
		assertSkipping("Main", DependencyDirty("Foo - Tainted server/invalidate"));
	}

	function testDefineModule(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.defineModule()"];
		runHaxe(args);
		assertSuccess();

		// Nothing is loading Bar, so no redefinition issue
		runHaxe(args);
		assertSuccess();
	}

	function testDefineModule1(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main1.hx"));
		var args = ["-main", "Main", "--macro", "Macro.defineModule()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		assertSuccess();
		// TODO: change invalidation reason
		assertSkipping("Main", DependencyDirty("Bar - Tainted server/invalidate"));
	}

	@:async
	@:timeout(3000)
	function testRedefineModule(async:Async) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main2.hx"));
		var args = ["-main", "Main", "--interp", "--macro", "Macro.redefineModule()"];
		var i = 0;
		function test() {
			runHaxe(args, () -> {
				assertSuccess();
				assertHasPrint("Foobar.test() = " + i);
				if (++i >= 5) async.done();
				else test();
			});
		}
		test();
	}

	function testAfterTyping(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.hook()"];
		runHaxe(args);
		assertSuccess();

		// Nothing is loading Baz, so it can be defined again
		runHaxe(args);
		assertSuccess();
	}

	@:async
	@:timeout(3000)
	function testRedefineAfterTyping(async:Async) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--interp", "--macro", "Macro.hookRedefine()"];
		var i = 0;
		function test() {
			runHaxe(args, () -> {
				assertSuccess();
				// Newest version is being included
				assertHasPrint("Foobaz.test() = " + i);
				if (++i >= 5) async.done();
				else test();
			});
		}
		test();
	}

	function testInvalidateError(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro1.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--interp", "--macro", "Macro.hookInvalidateError()"];
		runHaxe(args);
		assertErrorMessage("Cannot invalidate loaded module Empty");
	}

	function testInvalidateCaughtError(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro1.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--interp", "--macro", "Macro.hookInvalidateCatch()"];
		runHaxe(args);
		assertSuccess();
		assertHasPrint("Cannot invalidate loaded module Empty");
	}
}
