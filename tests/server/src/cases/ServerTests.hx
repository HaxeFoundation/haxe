package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import haxe.io.Path;
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
		assertSkipping("BuiltClass", DependencyDirty("BuildMacro - Tainted server/invalidate"));
		assertSkipping("BuildMacro", Tainted("server/invalidate"));
	}

	function testBrokenSyntaxDiagnostics() {
		vfs.putContent("BrokenSyntax.hx", getTemplate("BrokenSyntax.hx"));
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "BrokenSyntax.hx", "--interp", "--no-output"];
		runHaxe(args);
		assertErrorMessage("Expected }");
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Empty.hx")}, res -> {
			Assert.equals(0, res.length);
		});
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
		assertSuccess();
		assertSkipping("HelloWorld", Tainted("check_display_file"));

		runHaxe(args);
		assertSkipping("HelloWorld", Tainted("check_display_file"));
		// switch to this check when HxbData.always_wipe_cache = false
		// assertSkipping("HelloWorld", Tainted("server/invalidate"));
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

	function testDiagnosticsFileContents() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue9134/Main.hx"));
		vfs.putContent("Other.hx", getTemplate("issues/Issue9134/Other.hx"));
		var args = ["-main", "Main", "Other"];

		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {fileContents: [
			{file: new FsPath("Other.hx")},
			{file: new FsPath("Main.hx")},
		]}, res -> {
			Assert.equals(1, res.length);
			Assert.equals(1, res[0].diagnostics.length);
			var arg = res[0].diagnostics[0].args;
			Assert.equals("Unused variable", (cast arg).description);
			Assert.stringContains("Main.hx", res[0].file.toString());
		});

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Other.hx")});

		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {fileContents: [
			{file: new FsPath("Main.hx"), contents: getTemplate("issues/Issue9134/Main2.hx")},
			{file: new FsPath("Other.hx"), contents: getTemplate("issues/Issue9134/Other2.hx")}
		]}, res -> {
			Assert.equals(1, res.length);
			Assert.equals(1, res[0].diagnostics.length);
			var arg = res[0].diagnostics[0].args;
			Assert.equals("Unused variable", (cast arg).description);
			Assert.stringContains("Other.hx", res[0].file.toString());
		});

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Other.hx")});

		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {fileContents: [
			{file: new FsPath("Main.hx"), contents: getTemplate("issues/Issue9134/Main.hx")},
			{file: new FsPath("Other.hx"), contents: getTemplate("issues/Issue9134/Other2.hx")}
		]}, res -> {
			Assert.equals(2, res.length);

			for (i in 0...2) {
				Assert.equals(1, res[i].diagnostics.length);
				var arg = res[i].diagnostics[0].args;
				Assert.equals("Unused variable", (cast arg).description);
			}
		});

		// Currently, haxe compilation server will have this content anyway
		// because of diagnostics with file contents, but that behavior may not
		// be obvious in tests
		vfs.putContent("Other.hx", getTemplate("issues/Issue9134/Other2.hx"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Other.hx")});

		// Running project wide diagnostics; checks here aren't great since
		// results will depend on haxe std which may change without updating
		// this test everytime..
		runHaxeJsonCb(args, DisplayMethods.Diagnostics, {}, res -> {
			var hasMain = false;
			var hasOther = false;

			for (result in res) {
				var file = result.file.toString();
				if (StringTools.endsWith(file, "Main.hx")) hasMain = true;
				else if (StringTools.endsWith(file, "Other.hx")) hasOther = true;
				else continue;

				var arg = result.diagnostics[0].args;
				Assert.equals("Unused variable", (cast arg).description);
			}

			Assert.isTrue(hasMain);
			Assert.isTrue(hasOther);
		});
	}

	// function testDiagnosticsRecache() {
	// 	vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
	// 	var args = ["--main", "HelloWorld", "--interp"];
	// 	runHaxe(args);
	// 	assertSuccess();
	// 	runHaxe(args);
	// 	assertReuse("HelloWorld");
	// 	runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HelloWorld.hx")});
	// 	runHaxe(args);
	// 	assertSkipping("HelloWorld", Tainted("server/invalidate"));
	// 	runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("HelloWorld.hx")}, res -> {
	// 		Assert.equals(0, res.length);
	// 	});
	// 	runHaxe(args);
	// 	assertReuse("HelloWorld");
	// }

	// function testDiagnosticsRecache2() {
	// 	vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
	// 	var args = ["--main", "HelloWorld", "--interp"];
	// 	runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("HelloWorld.hx")}, res -> {
	// 		Assert.equals(0, res.length);
	// 	});
	// 	runHaxe(args);
	// 	assertReuse("HelloWorld");
	// }

	// function testDiagnosticsRecache3() {
	// 	vfs.putContent("HelloWorld.hx", getTemplate("HelloWorld.hx"));
	// 	var args = ["--main", "HelloWorld", "--interp"];
	// 	runHaxe(args);
	// 	runHaxe(args);
	// 	assertReuse("HelloWorld");
	// 	runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HelloWorld.hx")});
	// 	runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("HelloWorld.hx")}, res -> {
	// 		Assert.equals(0, res.length);
	// 	});
	// 	runHaxe(args.concat(["--display", "HelloWorld.hx@0@hover"]));
	// 	assertReuse("HelloWorld");
	// }

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

	function testMetadata() {
		var dummy_path = Path.join(["..", "misc", "projects", "Issue10844"]);
		Sys.command("haxelib", ["dev", "dummy_doc_dep", Path.join([dummy_path, "dummy_doc_dep"])]);
		Sys.command("haxelib", ["dev", "dummy_doc", Path.join([dummy_path, "dummy_doc"])]);
		var args = ["-lib", "dummy_doc"];

		runHaxeJsonCb(args, DisplayMethods.Metadata, {compiler: true, user: true}, function(meta) {
			var analyzer = Lambda.find(meta, m -> m.name == ':analyzer');
			Assert.notNull(analyzer);
			Assert.equals("Used to configure the static analyzer.", analyzer.doc);
			Assert.equals("haxe compiler", analyzer.origin);

			var dummy_doc = Lambda.find(meta, m -> m.name == ':foo');
			Assert.notNull(dummy_doc);
			Assert.equals("Some documentation for the @:foo metadata for cpp platform", dummy_doc.doc);
			Assert.equals("dummy_doc", dummy_doc.origin);
			Assert.equals(Platform.Cpp, dummy_doc.platforms[0]);

			var dummy_doc = Lambda.find(meta, m -> m.name == ':bar');
			Assert.notNull(dummy_doc);
			Assert.equals("dummy_doc", dummy_doc.origin);
			Assert.equals(MetadataTarget.Class, dummy_doc.targets[0]);

			var dummy_doc_dep = Lambda.find(meta, m -> m.name == ':baz');
			Assert.notNull(dummy_doc_dep);
			Assert.equals("dummy_doc_dep", dummy_doc_dep.origin);
		});

		runHaxeJsonCb(args, DisplayMethods.Metadata, {compiler: true, user: false}, function(meta) {
			var analyzer = Lambda.find(meta, m -> m.name == ':analyzer');
			Assert.notNull(analyzer);

			var dummy_doc = Lambda.find(meta, m -> m.name == ':foo');
			Assert.isNull(dummy_doc);

			var dummy_doc_dep = Lambda.find(meta, m -> m.name == ':baz');
			Assert.isNull(dummy_doc_dep);
		});

		runHaxeJsonCb(args, DisplayMethods.Metadata, {compiler: false, user: true}, function(meta) {
			var analyzer = Lambda.find(meta, m -> m.name == ':analyzer');
			Assert.isNull(analyzer);

			var dummy_doc = Lambda.find(meta, m -> m.name == ':foo');
			Assert.notNull(dummy_doc);

			var dummy_doc_dep = Lambda.find(meta, m -> m.name == ':baz');
			Assert.notNull(dummy_doc_dep);
		});

		runHaxeJsonCb(args, DisplayMethods.Metadata, {compiler: false, user: false}, function(meta) {
			Assert.equals(0, meta.length);
		});
	}

	function testDefines() {
		var dummy_path = Path.join(["..", "misc", "projects", "Issue10844"]);
		Sys.command("haxelib", ["dev", "dummy_doc_dep", Path.join([dummy_path, "dummy_doc_dep"])]);
		Sys.command("haxelib", ["dev", "dummy_doc", Path.join([dummy_path, "dummy_doc"])]);
		var args = ["-lib", "dummy_doc"];

		runHaxeJsonCb(args, DisplayMethods.Defines, {compiler: true, user: true}, function(defines) {
			var debug = Lambda.find(defines, d -> d.name == 'debug');
			Assert.notNull(debug);
			Assert.equals("Activated when compiling with -debug.", debug.doc);
			Assert.equals("haxe compiler", debug.origin);

			var dummy_doc = Lambda.find(defines, d -> d.name == 'no-bullshit');
			Assert.notNull(dummy_doc);
			Assert.equals("Only very important stuff should be compiled", dummy_doc.doc);
			Assert.equals("dummy_doc", dummy_doc.origin);

			var dummy_doc_dep = Lambda.find(defines, d -> d.name == 'dummy');
			Assert.notNull(dummy_doc_dep);
			Assert.equals("dummy_doc_dep", dummy_doc_dep.origin);
		});

		runHaxeJsonCb(args, DisplayMethods.Defines, {compiler: true, user: false}, function(defines) {
			var debug = Lambda.find(defines, d -> d.name == 'debug');
			Assert.notNull(debug);

			var dummy_doc = Lambda.find(defines, d -> d.name == 'no-bullshit');
			Assert.isNull(dummy_doc);

			var dummy_doc_dep = Lambda.find(defines, d -> d.name == 'dummy');
			Assert.isNull(dummy_doc_dep);
		});

		runHaxeJsonCb(args, DisplayMethods.Defines, {compiler: false, user: true}, function(defines) {
			var debug = Lambda.find(defines, d -> d.name == 'debug');
			Assert.isNull(debug);

			var dummy_doc = Lambda.find(defines, d -> d.name == 'no-bullshit');
			Assert.notNull(dummy_doc);

			var dummy_doc_dep = Lambda.find(defines, d -> d.name == 'dummy');
			Assert.notNull(dummy_doc_dep);
		});

		runHaxeJsonCb(args, DisplayMethods.Defines, {compiler: false, user: false}, function(defines) {
			Assert.equals(0, defines.length);
		});
	}

	function test10986() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue10986/Main.hx"));
		vfs.putContent("haxe/ds/Vector.hx", getTemplate("issues/Issue10986/Vector.hx"));
		var args = ["-main", "Main", "--jvm", "Main.jar"];
		runHaxe(args);
		vfs.touchFile("haxe/ds/Vector.hx");
		runHaxe(args);
		assertSuccess();
	}

	function test11179() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue11179/Main.hx"));
		var args = ["-main", "Main", "--macro", 'nullSafety("Main", Strict)', "--interp"];
		runHaxe(args);
		runHaxe(args);
		assertSuccess();
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

	function testXRedefinedFromX_2() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8368/Main2.hx"));
		var transform = Marker.extractMarkers(getTemplate("issues/Issue8368/MyMacro2.macro.hx"));
		var args = ["-main", "Main", "--macro", "define('whatever')"];

		vfs.putContent(
			"MyMacro.macro.hx",
			transform.source.substr(0, transform.markers[1])
			+ transform.source.substr(transform.markers[2], transform.source.length)
		);

		runHaxe(args);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("MyMacro.macro.hx")});

		var completionRequest = {file: new FsPath("MyMacro.macro.hx"), contents: transform.source, offset: transform.markers[2], wasAutoTriggered: false};
		runHaxeJson(args, DisplayMethods.Completion, completionRequest);
		Assert.isTrue(parseCompletion().result.items.length == 23);
		runHaxeJson(args, DisplayMethods.Completion, completionRequest);
		Assert.isTrue(parseCompletion().result.items.length == 23);
		runHaxeJson(args, DisplayMethods.Completion, completionRequest);
		Assert.isTrue(parseCompletion().result.items.length == 23);

		runHaxe(args);
		assertSuccess();
	}

	@:async function testStackOverflow(async:utest.Async) {
		vfs.putContent("Empty.hx", getTemplate("Empty.hx"));
		var args = ["-main", "Empty.hx", "--macro", "allowPackage('sys')", "--interp", "--no-output"];
		var runs = 0;

		function runLoop() {
			runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Empty.hx")}, () -> {
				runHaxe(args.concat(["-D", "compile-only-define"]), () -> {
					if (assertSuccess() && ++runs < 20) runLoop();
					else async.done();
				});
			});
		}

		async.setTimeout(20000);
		runLoop();
	}

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

	function testIssue8616() {
		vfs.putContent("Main.hx", getTemplate("issues/Issue8616/Main.hx"));
		vfs.putContent("A.hx", getTemplate("issues/Issue8616/A.hx"));
		var args = ["-main", "Main", "-js", "out.js"];
		runHaxe(args);
		var originalContent = sys.io.File.getContent(haxe.io.Path.join([testDir, "out.js"]));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		var content = sys.io.File.getContent(haxe.io.Path.join([testDir, "out.js"]));
		Assert.isTrue(content == originalContent);
	}

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
