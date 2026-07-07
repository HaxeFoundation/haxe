package cases;

import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Server;
import TestCase;
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
		var msgs = [for (d in diag) for (e in d.diagnostics) if (e.severity == Error) Std.string(e.args)];
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

	// Faithful replica of the alchimix module graph: B (HintButton) extends a shared base, reads a
	// static off C's class in a method body (mutual dep), and its CONSTRUCTOR gains ?font:Font (a type
	// from another module) inserted before ?onClick. C (NavBar) declares an anon typedef in its own
	// module and calls `new B(...)` in a loop over it, passing the old optional args (skip-required).
	// A (LeaderboardView) gains its FIRST dependency on B in the same edit, passing the new arg.
	static final fontMod = "class Font { public function new() {} }";
	static final assetsMod = "class Assets { public static var fontSmall = new Font(); public static var fontLarge = new Font(); }";
	static final flowMod = "class Flow { public function new() {} }";
	static final ctxMod = "class Ctx { public function new() {} }";
	static final actMod = "enum Act { Meta; Nav; }";
	static final hbOld = "class HintButton extends Flow {\n"
		+ "\tvar onClick:Void->Void;\n"
		+ "\tpublic function new(context:Ctx, action:Act, label:String, ?onClick:Void->Void, ?disabled:Bool = false) {\n"
		+ "\t\tsuper();\n"
		+ "\t\tthis.onClick = onClick;\n"
		+ "\t\tvar d = disabled || onClick == NavBar.noop;\n"
		+ "\t}\n"
		+ "}";
	static final hbNew = "class HintButton extends Flow {\n"
		+ "\tvar onClick:Void->Void;\n"
		+ "\tvar font:Font;\n"
		+ "\tpublic function new(context:Ctx, action:Act, label:String, ?font:Font, ?onClick:Void->Void, ?disabled:Bool = false) {\n"
		+ "\t\tsuper();\n"
		+ "\t\tthis.onClick = onClick;\n"
		+ "\t\tthis.font = font != null ? font : Assets.fontLarge;\n"
		+ "\t\tvar d = disabled || onClick == NavBar.noop;\n"
		+ "\t}\n"
		+ "}";
	static final navBar = "typedef NavEntry = {\n"
		+ "\tvar key:Act;\n"
		+ "\tvar label:String;\n"
		+ "\tvar action:Void->Void;\n"
		+ "\t@:optional var disabled:Bool;\n"
		+ "\t@:optional var visible:Bool;\n"
		+ "}\n"
		+ "class NavBar extends Flow {\n"
		+ "\tpublic static function noop() {}\n"
		+ "\tfinal context:Ctx;\n"
		+ "\tvar entries:Array<NavEntry>;\n"
		+ "\tpublic function new(context:Ctx) {\n"
		+ "\t\tsuper();\n"
		+ "\t\tthis.context = context;\n"
		+ "\t\tentries = [];\n"
		+ "\t}\n"
		+ "\tfunction refresh() {\n"
		+ "\t\tfor (e in entries) {\n"
		+ "\t\t\tif (!e.visible) continue;\n"
		+ "\t\t\tvar el = new HintButton(context, e.key, e.label, e.action, e.disabled);\n"
		+ "\t\t}\n"
		+ "\t}\n"
		+ "}";
	static final lvOld = "class LeaderboardView extends Flow {\n"
		+ "\tvar nav:NavBar;\n"
		+ "\tpublic function new(context:Ctx) {\n"
		+ "\t\tsuper();\n"
		+ "\t\tnav = new NavBar(context);\n"
		+ "\t}\n"
		+ "}";
	static final lvNew = "class LeaderboardView extends Flow {\n"
		+ "\tvar nav:NavBar;\n"
		+ "\tvar hint:HintButton;\n"
		+ "\tpublic function new(context:Ctx) {\n"
		+ "\t\tsuper();\n"
		+ "\t\tnav = new NavBar(context);\n"
		+ "\t\thint = new HintButton(context, Meta, \"Select\\nScope\", Assets.fontSmall, () -> trace(\"TODO\"));\n"
		+ "\t}\n"
		+ "}";
	static final bootMod = "class Boot { static function main() { new LeaderboardView(new Ctx()); } }";

	@:coroutine function scenarioFaithful(defines:Array<String>, label:String) {
		vfs.putContent("Font.hx", fontMod);
		vfs.putContent("Assets.hx", assetsMod);
		vfs.putContent("Flow.hx", flowMod);
		vfs.putContent("Ctx.hx", ctxMod);
		vfs.putContent("Act.hx", actMod);
		vfs.putContent("HintButton.hx", hbOld);
		vfs.putContent("NavBar.hx", navBar);
		vfs.putContent("LeaderboardView.hx", lvOld);
		vfs.putContent("Boot.hx", bootMod);
		var args = ["-main", "Boot", "-js", "no.js", "--no-output"].concat(defines);
		runHaxe(args);
		assertSuccess();

		// a display round so modules get served/restored through the display tiers
		var offset = navBar.indexOf("new HintButton(") + "new Hint".length;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("NavBar.hx"), offset: offset});
		diagnose(args, "LeaderboardView.hx", '$label/initial');

		// the edit: ?font inserted in B's ctor, A gains its first dep on B; C untouched
		vfs.putContent("HintButton.hx", hbNew);
		vfs.putContent("LeaderboardView.hx", lvNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HintButton.hx")});
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("LeaderboardView.hx")});
		diagnose(args, "LeaderboardView.hx", '$label/postEdit-A');
		diagnose(args, "NavBar.hx", '$label/postEdit-C');
		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertSuccess();
	}

	// IDE-shaped variant: no full compiles at all — the server only ever sees diagnostics and hover
	// rounds (vshaxe reality), with display.lazy_sibling_build also on.
	@:coroutine function scenarioFaithfulDisplayOnly(defines:Array<String>, label:String) {
		vfs.putContent("Font.hx", fontMod);
		vfs.putContent("Assets.hx", assetsMod);
		vfs.putContent("Flow.hx", flowMod);
		vfs.putContent("Ctx.hx", ctxMod);
		vfs.putContent("Act.hx", actMod);
		vfs.putContent("HintButton.hx", hbOld);
		vfs.putContent("NavBar.hx", navBar);
		vfs.putContent("LeaderboardView.hx", lvOld);
		vfs.putContent("Boot.hx", bootMod);
		var args = ["-main", "Boot", "-js", "no.js", "--no-output"].concat(defines);
		diagnose(args, "NavBar.hx", '$label/warmup-C');
		diagnose(args, "LeaderboardView.hx", '$label/warmup-A');
		var offset = navBar.indexOf("new HintButton(") + "new Hint".length;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("NavBar.hx"), offset: offset});

		// edit B, diagnostics round (A not yet edited — old A has no B dep at all)
		vfs.putContent("HintButton.hx", hbNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HintButton.hx")});
		diagnose(args, "HintButton.hx", '$label/afterB');

		// mid-typing states of A: requests that ERROR while B's fresh restore is still pending —
		// an aborted flush can freeze placeholder field data inside resident modules
		var lvBroken1 = lvNew.split("new HintButton(context, Meta, \"Select\\nScope\", Assets.fontSmall, () -> trace(\"TODO\"))").join("new HintButton(context, Meta)");
		var lvBroken2 = lvNew.split("Assets.fontSmall").join("Assets.fontSm");
		vfs.putContent("LeaderboardView.hx", lvBroken1);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("LeaderboardView.hx")});
		runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("LeaderboardView.hx")});
		vfs.putContent("LeaderboardView.hx", lvBroken2);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("LeaderboardView.hx")});
		runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("LeaderboardView.hx")});
		var hoverBroken = lvBroken2.indexOf("new HintButton(") + "new Hint".length;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("LeaderboardView.hx"), offset: hoverBroken, contents: lvBroken2});

		// edit A, diagnostics rounds
		vfs.putContent("LeaderboardView.hx", lvNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("LeaderboardView.hx")});
		diagnose(args, "LeaderboardView.hx", '$label/afterA');
		diagnose(args, "NavBar.hx", '$label/afterA-C');
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("NavBar.hx"), offset: offset});
		diagnose(args, "NavBar.hx", '$label/final-C');
	}

	// The alchimix build also runs an init macro on EVERY request that injects a @:build (with inline
	// field patches) onto the shared base class via Compiler.addGlobalMetadata (no-spoon style), so the
	// whole component hierarchy is macro-built. Mirror that on top of the faithful graph.
	static final benderMod = "import haxe.macro.Context;\nimport haxe.macro.Compiler;\n"
		+ "class Bender {\n"
		+ "\tpublic static function bend() {\n"
		+ "\t\tCompiler.addGlobalMetadata(\"Flow\", \"@:build(Bender.patch())\");\n"
		+ "\t}\n"
		+ "\tpublic static function patch() {\n"
		+ "\t\tvar fields = Context.getBuildFields();\n"
		+ "\t\tfields.push({name: \"bump\", pos: Context.currentPos(), access: [APublic, AInline],\n"
		+ "\t\t\tkind: FFun({args: [], expr: macro return 1})});\n"
		+ "\t\treturn fields;\n"
		+ "\t}\n"
		+ "}";

	@:coroutine function scenarioMacroBend(defines:Array<String>, label:String) {
		vfs.putContent("Font.hx", fontMod);
		vfs.putContent("Assets.hx", assetsMod);
		vfs.putContent("Flow.hx", flowMod);
		vfs.putContent("Ctx.hx", ctxMod);
		vfs.putContent("Act.hx", actMod);
		vfs.putContent("HintButton.hx", hbOld);
		vfs.putContent("NavBar.hx", navBar);
		vfs.putContent("LeaderboardView.hx", lvOld);
		vfs.putContent("Boot.hx", bootMod);
		vfs.putContent("Bender.hx", benderMod);
		var args = ["-main", "Boot", "-js", "no.js", "--no-output", "--macro", "Bender.bend()"].concat(defines);
		diagnose(args, "NavBar.hx", '$label/warmup-C');
		diagnose(args, "LeaderboardView.hx", '$label/warmup-A');
		var offset = navBar.indexOf("new HintButton(") + "new Hint".length;
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("NavBar.hx"), offset: offset});
		runHaxe(args);
		assertSuccess();

		vfs.putContent("HintButton.hx", hbNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("HintButton.hx")});
		diagnose(args, "HintButton.hx", '$label/afterB');

		vfs.putContent("LeaderboardView.hx", lvNew);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("LeaderboardView.hx")});
		diagnose(args, "LeaderboardView.hx", '$label/afterA');
		diagnose(args, "NavBar.hx", '$label/afterA-C');
		runHaxeJson(args, DisplayMethods.Hover, {file: new FsPath("NavBar.hx"), offset: offset});
		runHaxe(args);
		assertSuccess();
	}

	function testMacroBendAll(_) scenarioMacroBend([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation",
		"-D", "display.lazy_sibling_build"
	], "bend_all");

	function testMacroBendLazy(_) scenarioMacroBend(["-D", "hxb.lazy_inheritance"], "bend_lazy");

	function testMacroBendVanilla(_) scenarioMacroBend([], "bend_vanilla");

	function testFaithfulDisplayOnlyAll(_) scenarioFaithfulDisplayOnly([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation",
		"-D", "display.lazy_sibling_build"
	], "faithd_all");

	function testFaithfulDisplayOnlyLazy(_) scenarioFaithfulDisplayOnly([
		"-D", "hxb.lazy_inheritance"
	], "faithd_lazy");

	function testFaithfulAllLevers(_) scenarioFaithful([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation"
	], "faith_all");

	function testFaithfulLazy(_) scenarioFaithful(["-D", "hxb.lazy_inheritance"], "faith_lazy");

	function testFaithfulHeaderInv(_) scenarioFaithful(["-D", "hxb.header_invalidation"], "faith_header");

	function testFaithfulVanilla(_) scenarioFaithful([], "faith_vanilla");

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
		try {
			hoverCSub(args);
		} catch (e:TestException) {
			haxe.Log.trace('[$label] hoverCSub FAILED: ${e.message}', e.pos);
			debugMessages();
			debugErrorMessages();
			throw e;
		}
		diagnose(args, "C.hx", '$label/postEdit');
		diagnose(args, "A.hx", '$label/postEdit-A');
		runHaxe(args);
		assertSuccess();
		runHaxe(args);
		assertSuccess();
	}

	function testGenerationGapAllLevers(_) scenarioGenerationGap([
		"-D", "hxb.lazy_inheritance", "-D", "hxb.resident_modules", "-D", "hxb.header_invalidation",
		"-D", "hxb.header_invalidation_verbose"
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
