using StringTools;

@:timeout(5000)
class ServerTests extends HaxeServerTestCase {
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
		vfs.touchFile("HelloWorld.hx");
		runHaxe(args);
		assertSkipping("HelloWorld");
		assertNotCacheModified("HelloWorld");
	}

	function testDependency() {
		vfs.putContent("WithDependency.hx", getTemplate("WithDependency.hx"));
		vfs.putContent("Dependency.hx", getTemplate("Dependency.hx"));
		var args = ["-main", "WithDependency.hx", "--no-output", "-js", "no.js"];
		runHaxe(args);
		vfs.touchFile("Dependency.hx");
		runHaxe(args);
		assertSkipping("WithDependency", "Dependency");
		assertNotCacheModified("Dependency");
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
		vfs.touchFile("MacroMain.hx");
		runHaxe(args);
		assertHasPrint("1");
		vfs.touchFile("Macro.hx");
		runHaxe(args);
		assertHasPrint("1");
		vfs.putContent("Macro.hx", getTemplate("Macro.hx").replace("1", "2"));
		runHaxe(args);
		assertHasPrint("2");
		vfs.touchFile("MacroMain.hx");
		runHaxe(args);
		assertHasPrint("2");
	}

	function testDceEmpty() {
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty", "--no-output", "-java", "java"];
		runHaxe(args);
		runHaxeJson(args, "typer/compiledTypes", {});
		assertHasField("", "Type", "enumIndex", true);
	}

	function testBuildMacro() {
		vfs.putContent("BuildMacro.hx", getTemplate("BuildMacro.hx"));
		vfs.putContent("BuiltClass.hx", getTemplate("BuiltClass.hx"));
		var args = ["-main", "BuiltClass.hx", "--interp"];
		runHaxe(args);
		runHaxe(args);
		assertReuse("BuiltClass");
		vfs.touchFile("BuildMacro.hx");
		runHaxe(args);
		assertNotCacheModified("BuildMacro");
		assertSkipping("BuiltClass", "BuildMacro");
		assertSkipping("BuildMacro");
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
		vfs.touchFile("HelloWorld.hx");
		runHaxe(args2);

		runHaxe(args);
		assertSkipping("HelloWorld");
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

	function testSyntaxCache() {
		vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
		runHaxeJson(["-cp", "."], "server/readClassPaths", {});
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		runHaxeJson([], "display/completion", {file: "HelloWorld.hx", offset: 75, wasAutoTriggered: false});
		var completion = parseCompletion();
		assertHasCompletion(completion, module -> switch (module.kind) {
			case Type: module.args.path.typeName == "HelloWorld";
			case _: false;
		});
		// assertHasCompletion(completion, module -> switch (module.kind) {
		// 	case Type: module.args.path.typeName == "Empty";
		// 	case _: false;
		// });
	}
}

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");
		utest.UTest.run([new ServerTests()]);
	}
}
