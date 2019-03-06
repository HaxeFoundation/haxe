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
		runHaxe(args, true);
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
}

class Main {
	static public function main() {
		Vfs.removeDir("test/cases");
		utest.UTest.run([new ServerTests()]);
	}
}
