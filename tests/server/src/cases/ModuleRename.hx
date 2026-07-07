package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

using StringTools;
using Lambda;

// Field report (2026-07-07, alchimix): after a refactor renamed a module, compiles through the server
// fail with position-less "Type not found : <old.module.Name>" until restart. Probe: rename a module a
// dependent uses, update the dependent, invalidate, and expect the server to recover.
class ModuleRename extends TestCase {
	static final popinOld = "class Popin { public static function show() {} }";
	static final popinNew = "class PopinView { public static function show() {} }";
	static final aOld = "class A { static function main() { Popin.show(); } }";
	static final aNew = "class A { static function main() { PopinView.show(); } }";
	// an untouched module that also depends on the renamed one's DEPENDENT (extra cache tier traffic)
	static final cMod = "class C { public static function poke() {} }";

	@:coroutine function diagnose(args:Array<String>, file:String, label:String) {
		var diag = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath(file)});
		var msgs = [for (d in diag) for (e in d.diagnostics) if (e.severity == Error) Std.string(e.args)];
		Assert.equals(0, msgs.length, '$label: unexpected diagnostics in $file: ${msgs.join(" | ")}');
	}

	@:coroutine function scenario(defines:Array<String>, label:String) {
		vfs.putContent("Popin.hx", popinOld);
		vfs.putContent("A.hx", aOld);
		vfs.putContent("C.hx", cMod);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();
		// display round so modules travel through the display/restore tiers
		var offset = aOld.indexOf("Popin.") + 3;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset});
		diagnose(args, "A.hx", '$label/initial');

		// the rename: Popin.hx removed, PopinView.hx created, dependent updated
		vfs.removeFile("Popin.hx");
		vfs.putContent("PopinView.hx", popinNew);
		vfs.putContent("A.hx", aNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Popin.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		runHaxeJson([], ServerMethods.ModuleCreated, {file: new FsPath("PopinView.hx")});
		diagnose(args, "A.hx", '$label/postRename');
		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertSuccess();
	}

	// same rename, but the watcher misses the deletion: only the edited dependent gets invalidated
	@:coroutine function scenarioNoEvents(defines:Array<String>, label:String) {
		vfs.putContent("Popin.hx", popinOld);
		vfs.putContent("A.hx", aOld);
		vfs.putContent("C.hx", cMod);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();
		var offset = aOld.indexOf("Popin.") + 3;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("A.hx"), offset: offset});

		vfs.removeFile("Popin.hx");
		vfs.putContent("PopinView.hx", popinNew);
		vfs.putContent("A.hx", aNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertSuccess();
	}

	function testVanilla(_) scenario([], "vanilla");

	function testLazyInheritance(_) scenario(["-D", "hxb.lazy_inheritance"], "lazy");

	function testResidentModules(_) scenario(["-D", "hxb.resident_modules"], "resident");

	function testHeaderInvalidation(_) scenario(["-D", "hxb.header_invalidation"], "header_inv");

	function testAllLevers(_) scenario([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "all");

	function testNoEventsVanilla(_) scenarioNoEvents([], "noev_vanilla");

	function testNoEventsResident(_) scenarioNoEvents(["-D", "hxb.resident_modules"], "noev_resident");

	function testNoEventsAllLevers(_) scenarioNoEvents([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "noev_all");
}
