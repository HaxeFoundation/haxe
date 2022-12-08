package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

using StringTools;
using Lambda;

class ServerTests extends TestCase {
	function testNoModification() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["-main", "HelloWorld.hx", "--no-output", "-js", "no.js"];
		runHaxe(args);
		runHaxe(args);
		assertReuse("HelloWorld");
		runHaxe(args);
		assertReuse("HelloWorld");
	}

	function testModification() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["-main", "HelloWorld.hx", "--no-output", "-js", "no.js"];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HelloWorld.hx")});
		runHaxe(args);
		assertSkipping("HelloWorld", Tainted("server/invalidate"));
		// assertNotCacheModified("HelloWorld");
	}

	function testDependency() {
		vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = ["-main", "WithDependency.hx", "--no-output", "-js", "no.js"];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dependency.hx")});
		runHaxe(args);
		assertSkipping("WithDependency", DependencyDirty("Dependency - Tainted server/invalidate"));
		// assertNotCacheModified("Dependency");
		runHaxe(args);
		assertReuse("Dependency");
		assertReuse("WithDependency");
	}

	function testMacro() {
		vfs.putContent("MacroMain.hx", getTemplate("MacroMain.hx"));
		vfs.putContent("Macro.hx", getTemplate("Macro.hx"));
		var args = ["-main", "MacroMain.hx", "--no-output", "-js", "no.js"];
		runHaxe(args);
		assertHasPrint("1");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("MacroMain.hx")});
		runHaxe(args);
		assertHasPrint("1");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Macro.hx")});
		runHaxe(args);
		assertHasPrint("1");
		vfs.putContent("Macro.hx", getTemplate("Macro.hx").replace("1", "2"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Macro.hx")});
		runHaxe(args);
		assertHasPrint("2");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("MacroMain.hx")});
		runHaxe(args);
		assertHasPrint("2");
	}

	function testDceEmpty() {
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--no-output", "-java", "java"];
		runHaxe(args);
		runHaxeJson(args, cast "typer/compiledTypes" /* TODO */, {});
		assertHasField("", "Type", "enumIndex", true);
	}

	function testBuildMacro() {
		vfs.putContent("BuildMacro.hx", getTemplate("BuildMacro.hx"));
		vfs.putContent("BuiltClass.hx", getTemplate("BuiltClass.hx"));
		var args = ["-main", "BuiltClass.hx", "--interp"];
		runHaxe(args);
		runHaxe(args);
		/* This often fails on our CI because the reported stdout is empty. I don't know why this is the case,
			but it's probably some obscure timing issue related to pipes which has nothing to do with that we
			actually want to test here. */
		// trace(lastResult);
		// assertReuse("BuiltClass");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("BuildMacro.hx")});
		runHaxe(args);
		// assertNotCacheModified("BuildMacro");
		trace(lastResult);
		assertSkipping("BuiltClass", DependencyDirty("BuildMacro - Tainted server/invalidate"));
		assertSkipping("BuildMacro", Tainted("server/invalidate"));
	}

	function testBrokenSyntaxDiagnostics() {
		vfs.putContent("BrokenSyntax.hx", getTemplate("BrokenSyntax.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "BrokenSyntax.hx", "--interp", "--no-output"];
		runHaxe(args);
		assertErrorMessage("Expected }");
		runHaxe(args.concat(["--display", "Empty.hx@0@diagnostics"]));
		runHaxe(args);
		assertErrorMessage("Expected }");
	}

	function testGlobalBuildMacro_subsequentCompilations() {
		vfs.putContent("GlobalBuildMacro.hx", getTemplate("GlobalBuildMacro.hx"));
		var args = ["--macro", "GlobalBuildMacro.use()", "--run", "GlobalBuildMacro"];
		runHaxe(args);
		runHaxe(args);
		assertSuccess();
	}

	function testDisplayModuleRecache() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["--main", "HelloWorld", "--interp"];
		runHaxe(args);
		runHaxe(args);
		assertReuse("HelloWorld");

		var args2 = ["--main", "HelloWorld", "--interp", "--display", "HelloWorld.hx@64@type"];
		runHaxe(args2);

		runHaxe(args);
		assertReuse("HelloWorld");

		// make sure we still invalidate if the file does change
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HelloWorld.hx")});
		runHaxe(args2);

		runHaxe(args);
		assertSkipping("HelloWorld", Tainted("check_display_file"));
	}

	function testMutuallyDependent() {
		vfs.putContent("MutuallyDependent1.hx", getTemplate("MutuallyDependent1.hx"));
		vfs.putContent("MutuallyDependent2.hx", getTemplate("MutuallyDependent2.hx"));

		var args = ["MutuallyDependent1", "MutuallyDependent2"];
		runHaxe(args);

		args = args.concat(["--display", "MutuallyDependent1.hx@44@type"]);
		runHaxe(args);
		assertSuccess();
	}

	function testDiagnosticsRecache() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["--main", "HelloWorld", "--interp"];
		runHaxe(args);
		runHaxe(args);
		assertReuse("HelloWorld");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HelloWorld.hx")});
		runHaxe(args);
		assertSkipping("HelloWorld", Tainted("server/invalidate"));
		runHaxe(args.concat(["--display", "HelloWorld.hx@0@diagnostics"]));
		runHaxe(args);
		assertReuse("HelloWorld");
	}

	function testDiagnosticsRecache2() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["--main", "HelloWorld", "--interp"];
		runHaxe(args.concat(["--display", "HelloWorld.hx@0@diagnostics"]));
		runHaxe(args);
		assertReuse("HelloWorld");
	}

	function testDiagnosticsRecache3() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["--main", "HelloWorld", "--interp"];
		runHaxe(args);
		runHaxe(args);
		assertReuse("HelloWorld");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HelloWorld.hx")});
		runHaxe(args.concat(["--display", "HelloWorld.hx@0@diagnostics"]));
		runHaxe(args.concat(["--display", "HelloWorld.hx@0@hover"]));
		assertReuse("HelloWorld");
	}

	function testSyntaxCache() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		runHaxeJson(["-cp", "."], ServerMethods.ReadClassPaths, null);
		vfs.putContent("Empty.hx", "");
		runHaxeJson([], ServerMethods.ModuleCreated, {file: new FsPath("Empty.hx")});
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "HelloWorld";
			case _: false;
		});
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "Empty";
			case _: false;
		});
		// check removal
		vfs.putContent("Empty.hx", "");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Empty.hx")});
		runHaxeJson([], DisplayMethods.Completion, {file: new FsPath("HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "HelloWorld";
			case _: false;
		});
		assertHasNoCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "Empty";
			case _: false;
		});
	}

	function testSyntaxCache2() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		var args = ["-cp", ".", "--interp"];
		runHaxeJson(args, ServerMethods.ReadClassPaths, null);
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		runHaxeJson([] /* No args here because file watchers don't generally know */, ServerMethods.ModuleCreated, {file: new FsPath("Empty.hx")});
		runHaxeJson(args, DisplayMethods.Completion, {file: new FsPath("HelloWorld.hx"), offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "Empty";
			case _: false;
		});
	}

	function testVectorInliner() {
		vfs.putContent("Vector.hx", getTemplate("Vector.hx"));
		vfs.putContent("VectorInliner.hx", getTemplate("VectorInliner.hx"));
		var args = ["-main", "VectorInliner", "--interp"];
		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("VectorInliner.hx")});
		runHaxeJson(args, cast "typer/compiledTypes" /* TODO */, {});
		var type = getStoredType("", "VectorInliner");
		function moreHack(s:String) {
			return ~/[\r\n\t]/g.replace(s, "");
		}
		utest.Assert.equals("function() {_Vector.Vector_Impl_.toIntVector(null);}", moreHack(type.args.statics[0].expr.testHack)); // lmao
	}

	// See https://github.com/HaxeFoundation/haxe/issues/8368#issuecomment-525379060
	#if false
	function testXRedefinedFromX() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8368/Main.hx"));
		vfs.putContent("MyMacro.hx", getTemplate("issues/Issue8368/MyMacro.hx"));
		vfs.putContent("Type1.hx", getTemplate("issues/Issue8368/Type1.hx"));
		vfs.putContent("Type2.hx", getTemplate("issues/Issue8368/Type2.hx"));
		var args = ["-main", "Main", "--macro", "define('whatever')"];
		runHaxe(args);
		runHaxe(args);
		assertSuccess();
	}
	#end

	function testMacroStaticsReset() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8631/Main.hx"));
		vfs.putContent("Init.hx", getTemplate("issues/Issue8631/Init.hx"));
		vfs.putContent("Macro.hx", getTemplate("issues/Issue8631/Macro.hx"));
		var hxml = ["-main", "Main", "--macro", "Init.callMacro()", "--interp"];
		runHaxe(hxml);
		runHaxe(hxml);
		var counter = vfs.getContent("counter.txt");
		utest.Assert.equals('2', counter);
	}

	// function testIssue8616() {
	// 	vfs.putContent("Main.hx", getTemplate("issues/Issue8616/Main.hx"));
	// 	vfs.putContent("A.hx", getTemplate("issues/Issue8616/A.hx"));
	// 	var args = ["-main", "Main", "-js", "out.js"];
	// 	runHaxe(args);
	// 	runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
	// 	runHaxe(args);
	// 	var content = sys.io.File.getContent(haxe.io.Path.join([testDir, "out.js"]));
	// 	Assert.isTrue(content.indexOf("this1.use(v1)") != -1);
	// }

	function test9918() {
		vfs.putContent("Issue9918.hx", getTemplate("Issue9918.hx"));
		var args = ["-main", "Issue9918.hx"];
		runHaxe(args);
		assertHasPrint('Issue9918.hx:22: correct ECast count');
		runHaxe(args);
		assertHasPrint('Issue9918.hx:22: correct ECast count');
		runHaxe(args);
		assertHasPrint('Issue9918.hx:22: correct ECast count');
	}
}
