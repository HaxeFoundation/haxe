package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

// Field report (alchimix, 2026-07-06): B gains a new optional arg inserted BEFORE its existing
// optional args; A (which calls B.foo) is updated for the new signature; C is untouched and calls
// B.foo with the old optional args (still valid via optional-arg skipping — a clean compile passes).
// After invalidating only A and B, C failed with a unification error on the server.
// The arg types are chosen so the old-args call REQUIRES optional-arg skipping ("x" does not unify
// with the inserted ?n:Int), mirroring the report's "X should be Y" error (first value vs inserted
// param) — i.e. skipping did not happen server-side.
class StaleOptionalArgs extends TestCase {
#if !disable_hxb_cache
	static final bOld = "class B { public static function foo(?a:String, ?b:Bool) {} }";
	static final bNew = "class B { public static function foo(?n:Int, ?a:String, ?b:Bool) {} }";
	static final aOld = "class A { static function main() { B.foo(\"y\", true); C.test(); } }";
	static final aNew = "class A { static function main() { B.foo(5, \"y\", true); C.test(); } }";
	static final c = "class C { public static function test() { B.foo(\"x\", true); } }";

	@:coroutine function diagnose(args:Array<String>, file:String, label:String) {
		var diag = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath(file)});
		var msgs = [for (d in diag) for (e in d.diagnostics) Std.string(e.args)];
		Assert.equals(0, msgs.length, '$label: unexpected diagnostics in $file: ${msgs.join(" | ")}');
	}

	@:coroutine function hoverC(args:Array<String>) {
		var offset = c.indexOf("foo(") + 2;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("C.hx"), offset: offset, contents: c});
	}

	// IDE-shaped flow: compile, display activity on C, staggered edits of B then A (each with its
	// own invalidate + diagnostics round, like save-triggered diagnostics), final compile.
	@:coroutine function scenario(defines:Array<String>, label:String) {
		vfs.putContent("A.hx", aOld);
		vfs.putContent("B.hx", bOld);
		vfs.putContent("C.hx", c);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();
		hoverC(args);
		diagnose(args, "C.hx", '$label/initial');

		// edit B first (A still on the old call — silent skipping keeps it valid)
		vfs.putContent("B.hx", bNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("B.hx")});
		diagnose(args, "B.hx", '$label/afterB');

		// then edit A for the new signature
		vfs.putContent("A.hx", aNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		diagnose(args, "A.hx", '$label/afterA');
		diagnose(args, "C.hx", '$label/afterA-C');

		runHaxe(args);
		assertSuccess();
		hoverC(args);
		runHaxe(args);
		assertSuccess();
	}

	// Same report, member-method shape: foo lives on a BASE class of B (lazy_inheritance stubs sit on
	// the hierarchy walk), C calls it on an instance. Display request lands between invalidate and the
	// recompile, like an IDE hover racing save-triggered diagnostics.
	static final baseOld = "class BBase { public function new() {} public function foo(?a:String, ?b:Bool) {} }";
	static final baseNew = "class BBase { public function new() {} public function foo(?n:Int, ?a:String, ?b:Bool) {} }";
	static final bSub = "class B extends BBase {}";
	static final aSubOld = "class A { static function main() { new B().foo(\"y\", true); C.test(); } }";
	static final aSubNew = "class A { static function main() { new B().foo(5, \"y\", true); C.test(); } }";
	static final cSub = "class C { public static function test() { new B().foo(\"x\", true); } }";

	@:coroutine function hoverCSub(args:Array<String>) {
		var offset = cSub.indexOf("foo(") + 2;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("C.hx"), offset: offset, contents: cSub});
	}

	@:coroutine function scenarioInherited(defines:Array<String>, label:String) {
		vfs.putContent("A.hx", aSubOld);
		vfs.putContent("BBase.hx", baseOld);
		vfs.putContent("B.hx", bSub);
		vfs.putContent("C.hx", cSub);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();
		hoverCSub(args);

		vfs.putContent("BBase.hx", baseNew);
		vfs.putContent("A.hx", aSubNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("BBase.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		// display request BEFORE any full typing round: mints stubs from whatever tier serves them
		hoverCSub(args);
		diagnose(args, "C.hx", '$label/postInvalidate');
		runHaxe(args);
		assertSuccess();
		hoverCSub(args);
		runHaxe(args);
		assertSuccess();
	}

	// User hint: B itself DEPENDS on C (mutual dependency). Retyping B then re-enters C while B is
	// only partially rebuilt, so C may type B.foo against a not-yet-finalized signature.
	static final bCycOld = "class B { public static function foo(?a:String, ?b:Bool) { C.log(); } }";
	static final bCycNew = "class B { public static function foo(?n:Int, ?a:String, ?b:Bool) { C.log(); } }";
	static final cCyc = "class C { public static function log() {} public static function test() { B.foo(\"x\", true); } }";

	@:coroutine function scenarioCyclic(defines:Array<String>, label:String) {
		vfs.putContent("A.hx", aOld);
		vfs.putContent("B.hx", bCycOld);
		vfs.putContent("C.hx", cCyc);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();
		hoverC(args);
		diagnose(args, "C.hx", '$label/initial');

		vfs.putContent("B.hx", bCycNew);
		vfs.putContent("A.hx", aNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("B.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		diagnose(args, "C.hx", '$label/postInvalidate');
		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertSuccess();
	}

	// Kitchen sink: foo on BBase, B extends BBase and depends on C, A depends on B and C, and C's
	// typed binary is left several generations behind (intermediate A-only edit rounds) before the
	// signature edit lands — matching the restored-from-hxb precondition of earlier edge bugs.
	@:coroutine function scenarioGenerationGap(defines:Array<String>, label:String) {
		var bDep = "class B extends BBase { public static function poke() { C.log(); } }";
		var cGen = "class C { public static function log() {} public static function test() { new B().foo(\"x\", true); } }";
		var aGen1 = "class A { static function main() { new B().foo(\"y\", true); B.poke(); C.test(); } }";
		var aGen2 = "class A { static function main() { new B().foo(\"z\", true); B.poke(); C.test(); } }";
		var aGen3 = "class A { static function main() { new B().foo(7, \"z\", true); B.poke(); C.test(); } }";
		vfs.putContent("A.hx", aGen1);
		vfs.putContent("BBase.hx", baseOld);
		vfs.putContent("B.hx", bDep);
		vfs.putContent("C.hx", cGen);
		var args = ["-main", "A", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();
		hoverCSub(args);

		// intermediate generation: A-only body edit, C reused/restored
		vfs.putContent("A.hx", aGen2);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		diagnose(args, "A.hx", '$label/gen2');
		runHaxe(args);
		assertSuccess();

		// signature edit on BBase + A updated; C untouched since generation 1
		vfs.putContent("BBase.hx", baseNew);
		vfs.putContent("A.hx", aGen3);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("BBase.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("A.hx")});
		hoverCSub(args);
		diagnose(args, "C.hx", '$label/postEdit');
		diagnose(args, "A.hx", '$label/postEdit-A');
		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertSuccess();
	}

	function testGenerationGapAllLevers(_) scenarioGenerationGap([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "gen_all");

	function testGenerationGapLazy(_) scenarioGenerationGap(["-D", "hxb.lazy_inheritance"], "gen_lazy");

	function testVanilla(_) scenario([], "vanilla");

	function testCyclicVanilla(_) scenarioCyclic([], "cyc_vanilla");

	function testCyclicHeaderInvalidation(_) scenarioCyclic(["-D", "hxb.header_invalidation"], "cyc_header");

	function testCyclicLazyInheritance(_) scenarioCyclic(["-D", "hxb.lazy_inheritance"], "cyc_lazy");

	function testCyclicAllLevers(_) scenarioCyclic([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "cyc_all");

	function testInheritedLazy(_) scenarioInherited(["-D", "hxb.lazy_inheritance"], "inh_lazy");

	function testInheritedAllLevers(_) scenarioInherited([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "inh_all");

	function testHeaderInvalidation(_) scenario(["-D", "hxb.header_invalidation"], "header_inv");

	function testLazyInheritance(_) scenario(["-D", "hxb.lazy_inheritance"], "lazy_inh");

	function testAllLevers(_) scenario([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "all_levers");
#end
}
