package cases;

import haxe.io.Path;

class UnchangedContent extends TestCase {
	static function main(s:String) {
		return 'class Main { static function main() Sys.println("$s"); }';
	}

	function hasSkipping(module:String, reason:String) {
		return messages.exists(m -> m.contains('skipping $module ($reason'));
	}

	/*
		Absolute class paths, and a signature for each test: the server keeps the directories
		of the class paths by signature, and reads them outside of the directory of the request.
	*/
	function compileArgs(classPaths:Array<String>) {
		var args = [];
		for (cp in classPaths)
			args = args.concat(["-cp", Path.join([TestCase.rootCwd, testDir, cp])]);
		return args.concat(["-main", "Main", "--interp", "-D", 'test_dir=$testDir']);
	}

	/* the directories of the class paths are only known to the server after the second compilation */
	@:coroutine
	function compileTwice(args:Array<String>) {
		runHaxe(args);
		runHaxe(args);
	}

	function testSameContent() {
		vfs.putContent("src/Main.hx", main("D"));
		var args = compileArgs(["src"]);
		compileTwice(args);
		vfs.putContent("src/Main.hx", main("D"));
		vfs.touch("src/Main.hx");
		runHaxe(args);
		assertReuse("Main");
		assertHasPrint("D");
		runHaxe(args);
		assertReuse("Main");
	}

	function testChangedContent() {
		vfs.putContent("src/Main.hx", main("D"));
		var args = compileArgs(["src"]);
		compileTwice(args);
		vfs.putContent("src/Main.hx", main("E"));
		vfs.touch("src/Main.hx");
		runHaxe(args);
		Assert.isTrue(hasSkipping("Main", "FileChanged"));
		assertHasPrint("E");
	}

	function testEditorContentsThenRevert() {
		vfs.putContent("src/Main.hx", main("D"));
		var args = compileArgs(["src"]);
		compileTwice(args);
		assertHasPrint("D");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("src/Main.hx")});
		runHaxeJson(args, DisplayMethods.Diagnostics, {
			fileContents: [{file: new FsPath("src/Main.hx"), contents: main("E")}]
		});
		vfs.putContent("src/Main.hx", main("D"));
		vfs.touch("src/Main.hx");
		runHaxe(args);
		Assert.isFalse(hasMessage("reusing Main"));
		assertHasPrint("D");
	}

	/* contents sent with the request win over a file that was only written again */
	function testEditorContentsOverSameFile() {
		vfs.putContent("src/Main.hx", main("D"));
		var args = compileArgs(["src"]);
		compileTwice(args);
		vfs.putContent("src/Main.hx", main("D"));
		vfs.touch("src/Main.hx");
		runHaxeJson(args, DisplayMethods.Diagnostics, {
			fileContents: [{file: new FsPath("src/Main.hx"), contents: main("E")}]
		});
		Assert.isFalse(hasMessage("reusing Main"));
	}

	/* without module checks, the changed directory is what tells that the file changed */
	function testChangedContentWithoutModuleChecks() {
		var main = "class Main { static function main() Sys.println(Dep.value); }";
		vfs.putContent("src/Dep.hx", "class Dep { public static var value = 1; }");
		vfs.putContent("src/Main.hx", main);
		var args = compileArgs(["src"]);
		compileTwice(args);
		runHaxeJson(args, ServerMethods.Configure, {noModuleChecks: true});
		vfs.putContent("src/Other.hx", "class Other {}");
		vfs.putContent("src/Dep.hx", "class Dep { public static var value = 2; }");
		vfs.touch("src/Dep.hx");
		vfs.touch("src");
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("src/Main.hx"), offset: main.indexOf("value")});
		var skipped = hasSkipping("Main", "DependencyDirty Dep - Shadowed");
		runHaxeJson(args, ServerMethods.Configure, {noModuleChecks: false});
		Assert.isTrue(skipped);
	}

	function testMacroSameContent() {
		var macroSource = "class Macro { public static macro function get() return macro 1; }";
		vfs.putContent("src/Macro.hx", macroSource);
		vfs.putContent("src/Main.hx", "class Main { static function main() Sys.println(Macro.get()); }");
		var args = compileArgs(["src"]);
		compileTwice(args);
		vfs.putContent("src/Other.hx", "class Other {}");
		vfs.putContent("src/Macro.hx", macroSource);
		vfs.touch("src/Macro.hx");
		vfs.touch("src");
		runHaxe(args);
		assertReuse("Macro");
		Assert.isFalse(messages.exists(m -> m.contains("skipping Macro")));
	}

	function testShadowedByEarlierClassPath() {
		vfs.putContent("a/Other.hx", "class Other {}");
		vfs.putContent("b/Main.hx", main("D"));
		var args = compileArgs(["a", "b"]);
		compileTwice(args);
		assertHasPrint("D");
		vfs.putContent("a/Main.hx", main("E"));
		vfs.touch("a/Main.hx");
		vfs.touch("a");
		runHaxe(args);
		Assert.isTrue(hasSkipping("Main", "Shadowed"));
	}

	function testOwnFileInChangedDirectory() {
		vfs.putContent("src/Main.hx", main("D"));
		var args = compileArgs(["src"]);
		compileTwice(args);
		vfs.putContent("src/Other.hx", "class Other {}");
		vfs.putContent("src/Main.hx", main("D"));
		vfs.touch("src/Main.hx");
		vfs.touch("src");
		runHaxe(args);
		assertReuse("Main");
		assertHasPrint("D");
	}

	function testImportHxSameContent() {
		vfs.putContent("src/import.hx", "import haxe.ds.StringMap;");
		vfs.putContent("src/Main.hx", main("D"));
		var args = compileArgs(["src"]);
		compileTwice(args);
		vfs.putContent("src/import.hx", "import haxe.ds.StringMap;");
		vfs.touch("src/import.hx");
		runHaxe(args);
		assertReuse("Main");
	}
}
