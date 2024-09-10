package cases;

#if !enabled
class CsSafeTypeBuilding extends TestCase {
}
#else
import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

using StringTools;
using Lambda;

class CsSafeTypeBuilding extends TestCase {
	var originalContent:String;

	override public function setup(async:utest.Async) {
		super.setup(async);

		originalContent = "";
		vfs.putContent("Bar.hx", getTemplate("csSafeTypeBuilding/Bar.hx"));
		vfs.putContent("Baz.hx", getTemplate("csSafeTypeBuilding/Baz.hx"));
		vfs.putContent("Foo.hx", getTemplate("csSafeTypeBuilding/Foo.hx"));
		vfs.putContent("Macro.macro.hx", getTemplate("csSafeTypeBuilding/Macro.macro.hx"));
		vfs.putContent("Main.hx", getTemplate("csSafeTypeBuilding/Main.hx"));
	}

	#if debug
	var failed:Bool;
	function _assertHasPrint(s:String, ?pos:haxe.PosInfos) {
		if (!assertHasPrint(s)) {
			failed = true;
			haxe.Log.trace("Fail: doesn't contain \"" + s + "\"", pos);
		}
	}
	#end

	function assertResult(target:String) {
		#if debug
		failed = false;
		var assertHasPrint = _assertHasPrint;
		#end
		assertSuccess();

		// Make sure all types are generated
		assertHasPrint("[runtime] Hello from Bar");
		assertHasPrint("[runtime] Hello from Baz");
		assertHasPrint("[runtime] Hello from Foo__Bar__Bar");
		assertHasPrint("[runtime] Hello from Foo__Baz__Baz");
		assertHasPrint("[runtime] Hello from Foo__Main__Main");
		assertHasPrint("[runtime] Hello from Main");

		#if debug
		if (failed) messages.filter(m -> StringTools.startsWith(m, "Haxe print: ")).iter(m -> trace(m));
		#end

		// Disabled this check because types move around a bit so we get false negatives
		// Kept for debugging purposes
		if (false && target == "js") {
			var content = sys.io.File.getContent(haxe.io.Path.join([testDir, "out.js"]));
			Assert.isTrue(content == originalContent);

			// Needs https://github.com/kLabz/hxdiff for displaying diff
			// if (content != originalContent) {
			// 	final a = new diff.FileData(haxe.io.Bytes.ofString(originalContent), "expected", Date.now());
			// 	final b = new diff.FileData(haxe.io.Bytes.ofString(content), "actual", Date.now());
			// 	var ctx:diff.Context = {
			// 	  file1: a,
			// 	  file2: b,
			// 	  context: 10
			// 	}

			// 	final script = diff.Analyze.diff2Files(ctx);
			// 	var diff = diff.Printer.printUnidiff(ctx, script);
			// 	Sys.println(diff);
			// }
		}
	}

	function assertBuilt(modules:Array<String>, ?macroInvalidated:Bool = false) {
		#if debug trace('Invalidated ${modules.join(",")} (macro invalidated: ${macroInvalidated ? "true" : "false"})'); #end
		#if debug var assertHasPrint = _assertHasPrint; #end

		for (m in modules) {
			assertHasPrint('Building $m.');

			var t = 'Foo__${m}__${m}';
			if (!macroInvalidated) assertHasPrint('[$m] Previously generated type for $t has been discarded.');
			assertHasPrint('[$m] Generating type for $t.');

			if (m == "Baz") {
				assertHasPrint('[$m] Reusing previously generated type for Foo__Bar__Bar.');
			}
		}
	}

	@:variant("Js_DefineModule_GetType", true, true, "js")
	@:variant("Js_DefineType_GetType", false, true, "js")
	@:variant("Js_DefineModule_ResolveType", true, false, "js")
	@:variant("Js_DefineType_ResolveType", false, false, "js")
	@:variant("Interp_DefineModule_GetType", true, true, "interp")
	@:variant("Interp_DefineType_GetType", false, true, "interp")
	@:variant("Interp_DefineModule_ResolveType", true, false, "interp")
	@:variant("Interp_DefineType_ResolveType", false, false, "interp")
	function test(defineModule:Bool, getType:Bool, target:String) {
		var targetArgs = switch target {
			case "js": ["-js", "out.js", "-lib", "hxnodejs", "-cmd", "node out.js"];
			case "interp": ["--interp"];
			case _: [];
		}

		var args = ["-main", "Main", "Baz"];
		if (defineModule) args = args.concat(["-D", "config.defineModule"]);
		if (getType) args = args.concat(["-D", "config.getType"]);
		args = args.concat(targetArgs);

		runHaxe(args);
		if (target == "js") originalContent = sys.io.File.getContent(haxe.io.Path.join([testDir, "out.js"]));
		assertBuilt(["Main", "Bar", "Baz"], true);
		assertResult(target);

		#if debug trace("Rerun without invalidate"); #end
		runHaxe(args);
		assertResult(target);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Baz.hx")});
		runHaxe(args);
		assertBuilt(["Baz"]);
		assertResult(target);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertBuilt(["Main"]);
		assertResult(target);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Bar.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertBuilt(["Main", "Bar"]);
		assertResult(target);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Bar.hx")});
		runHaxe(args);
		assertBuilt(["Main", "Bar", "Baz"]);
		assertResult(target);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Foo.hx")});
		runHaxe(args);
		assertBuilt(["Main", "Bar", "Baz"]);
		assertResult(target);

		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Macro.macro.hx")});
		runHaxe(args);
		assertBuilt(["Main", "Bar", "Baz"], true);
		assertResult(target);
	}
}
#end
