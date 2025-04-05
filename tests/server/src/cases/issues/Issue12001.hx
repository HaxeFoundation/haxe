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

	function testRedefineTypeCatchError(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.redefineTypeCatchError()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		assertSuccess();
		assertHasPrint("Macro.hx:56: TInst(Foobar,[])");
		assertHasPrint("Macro.hx:69: Cannot redefine module Foobar");
	}

	@:async
	@:timeout(3000)
	function testRedefineType(async:Async) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main.hx"));
		var args = ["-main", "Main", "--interp", "--macro", "Macro.defineType()"];
		var i = 0;
		function test() {
			// Was failing with nightlies (HxbFailure)
			runHaxe(args, () -> {
				assertSuccess();
				assertHasPrint("Foo.test() = " + i);
				if (++i >= 5) async.done();
				else test();
			});
		}
		test();
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

	function testRedefineModuleCatchError(_) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--macro", "Macro.redefineModuleCatchError()"];
		runHaxe(args);
		assertSuccess();

		runHaxe(args);
		assertSuccess();
		assertHasPrint("Macro.hx:77: TInst(Foobaz,[])");
		assertHasPrint("Macro.hx:90: Cannot redefine module Foobaz");
	}

	@:async
	@:timeout(3000)
	function testRedefineModule(async:Async) {
		vfs.putContent("Macro.hx", getTemplate("issues/Issue12001/Macro.hx"));
		vfs.putContent("Main.hx", getTemplate("issues/Issue12001/Main1.hx"));
		var args = ["-main", "Main", "--interp", "--macro", "Macro.defineModule()"];
		var i = 0;
		function test() {
			// Was failing with nightlies (HxbFailure)
			runHaxe(args, () -> {
				assertSuccess();
				assertHasPrint("Bar.test() = " + i);
				if (++i >= 5) async.done();
				else test();
			});
		}
		test();
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
				assertHasPrint("Baz.test() = " + i);
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
