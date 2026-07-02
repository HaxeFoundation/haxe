package cases;

import haxe.display.FsPath;
import haxe.display.Server;
import haxe.io.Path;
import TestCase;
import utest.Assert;

// Module signatures + header invalidation are hxb-cache features: sparing/reuse only happens with the cache
// on, so these tests are meaningless (and assertReuse fails) under -D disable-hxb-cache. Exclude that variant.
// (Class kept defined for `addCases`; only its methods are guarded.)
class ModuleSignature extends TestCase {
#if !disable_hxb_cache
	// The signature dumped for a module restored from the hxb cache must be byte-identical to the one
	// dumped when the module was freshly typed. This is the determinism precondition for using module
	// signatures as the module-header diff layer: in particular, the var ids the hxb reader reassigns
	// to a restored module's expressions must normalize away (impl-field bodies are rendered with
	// positional var tokens), and no compiler-stage-derived meta may leak in. Also covers persistence:
	// the restored Dep dumps its stored m_sig, which must equal the freshly-typed one.
	function testSignatureStableAcrossCacheRestore() {
		vfs.putContent("Dep.hx", depContent());
		vfs.putContent("Main.hx", mainContent("// v1"));
		// Dump at `inlining`: cf_expr_unoptimized exists there, so impl-field bodies are captured.
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "dump=signatures", "-D", "dump.stage=inlining"];

		// First compile: Dep is freshly typed.
		runHaxe(args);
		assertSuccess();
		var fresh = depSignature();

		// Edit only Main, so on recompile Dep is reused from the cache (restored, not re-typed).
		vfs.putContent("Main.hx", mainContent("// v2"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Main.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("Dep");
		var restored = depSignature();

		Assert.equals(fresh, restored);
	}

	// The behaviour-neutral measurement (-D hxb.measure_signatures) must classify a re-typed module as
	// header-unchanged after a body-only edit (its dependents were dragged needlessly) and as
	// header-changed after a signature edit.
	function testMeasureSignatureInvalidation() {
		vfs.putContent("Dep.hx", dep("Int", "v + n"));
		vfs.putContent("Main.hx", "class Main { static function main() { trace(new Dep().bump(1)); } }");
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "hxb.measure_signatures"];

		runHaxe(args);
		assertSuccess();

		// Body-only edit: Dep's header is unchanged, so nothing should count as header-changed.
		vfs.putContent("Dep.hx", dep("Int", "v + n + 7"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.equals(0, measureChangedCount());

		// Signature edit (return type Int -> Float): header changed.
		vfs.putContent("Dep.hx", dep("Float", "v + n + 7"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isTrue(measureChangedCount() > 0);
	}

	// With -D hxb.header_invalidation: invalidating a seed whose header is unchanged (body-only edit)
	// must SPARE its dependents (they are reused, not re-typed); a signature edit must re-type the
	// dependents that observe the change.
	function testHeaderInvalidationSparesDependents() {
		vfs.putContent("Dep.hx", dep("Int", "v + n"));
		vfs.putContent("Main.hx", "class Main { static function main() { trace(new Dep().bump(1)); } }");
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "hxb.header_invalidation"];
		runHaxe(args);
		assertSuccess();

		// Body-only edit on the seed Dep: its header is unchanged, so Main must be spared (reused).
		vfs.putContent("Dep.hx", dep("Int", "v + n + 7"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("Main");

		// Signature edit on the seed (return type Int -> Float): Main observes it, so it is re-typed.
		vfs.putContent("Dep.hx", dep("Float", "v + n + 7"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Main"));
	}

	// Field-granular precision: Main uses only Dep.b. Changing Dep.a's signature must spare Main;
	// changing Dep.b's signature must re-type it.
	function testHeaderInvalidationFieldGranular() {
		vfs.putContent("Dep.hx", twoFn("Int", "Int"));
		vfs.putContent("Main.hx", "class Main { static function main() { trace(Dep.b(1)); } }");
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "hxb.header_invalidation"];
		runHaxe(args);
		assertSuccess();

		// Change only a's signature; Main uses only b -> spared.
		vfs.putContent("Dep.hx", twoFn("Float", "Int"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("Main");

		// Change b's signature; Main uses b -> re-typed.
		vfs.putContent("Dep.hx", twoFn("Int", "Float"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Main"));
	}

	// An inline static var/method is FOLDED into its callers, so the TField node that would yield a
	// field-granular dependency edge is erased. A change to the inlined value/body is still part of the
	// module signature, so the dependent must be re-typed -- otherwise it keeps the stale fold. Const.K's
	// value (1 -> 2) and inline fn's body both change here; Use folds both and must be re-typed.
	function testHeaderInvalidationInlineFold() {
		vfs.putContent("Const.hx", 'class Const {
	public static inline var K:Int = 1;
	public static inline function f(n:Int):Int return n + 10;
}');
		vfs.putContent("Use.hx", 'class Use {
	public static function valueK():Int return Const.K + 100;
	public static function valueF():Int return Const.f(1);
}');
		vfs.putContent("Main.hx", "class Main { static function main() { trace(Use.valueK() + Use.valueF()); } }");
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "hxb.header_invalidation"];
		runHaxe(args);
		assertSuccess();

		// Inline VALUE change (no field added/removed): Use folded K, so it must be re-typed.
		vfs.putContent("Const.hx", 'class Const {
	public static inline var K:Int = 2;
	public static inline function f(n:Int):Int return n + 10;
}');
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Const.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Use"));

		// Inline BODY change on the inline function: Use inlined f, so it must be re-typed.
		vfs.putContent("Const.hx", 'class Const {
	public static inline var K:Int = 2;
	public static inline function f(n:Int):Int return n + 20;
}');
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Const.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Use"));
	}

	// A @:build macro that reads a type via Context.getType has only a coarse module-level dependency
	// on it (no field-granular edge), so a header change to the read type would be ignored by the
	// field-granular sparing unless that dependency is recorded as a macro dependency. Editing Data's
	// header must re-type the build-macro target Generated (the macro could emit different code).
	function testHeaderInvalidationMacroGetType() {
		vfs.putContent("BuildMacro.macro.hx", 'import haxe.macro.Context;
class BuildMacro {
	public static function build():Array<haxe.macro.Expr.Field> {
		var fields = Context.getBuildFields();
		switch (Context.getType("Data")) {
			case TInst(_, _):
			case _:
		}
		return fields;
	}
}');
		vfs.putContent("Data.hx", "class Data { public var a:Int; public function new() a = 0; }");
		vfs.putContent("Generated.hx", "@:build(BuildMacro.build()) class Generated { public function new() {} }");
		vfs.putContent("Main.hx", "class Main { static function main() { new Generated(); new Data(); } }");
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "hxb.header_invalidation"];
		runHaxe(args);
		assertSuccess();

		// Header change on the macro-read type: Generated must be re-typed, not reused.
		vfs.putContent("Data.hx", "class Data { public var a:Int; public var b:Int; public function new() { a = 0; b = 0; } }");
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Data.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Generated"));
	}

	// Transitive field-granular soundness: a leaf change that propagates through a MID module whose own
	// signature changes. Chain: Leaf.X -> Mid.getX() (inferred return reads Leaf.X) -> ConsumerA (calls
	// Mid.getX, observes Mid's delta) ; ConsumerB calls only Mid.other (header-stable, must be spared).
	// Changing Leaf.X's type re-types Mid (its getX return type changes), cascades to ConsumerA, and
	// must spare ConsumerB. This exercises the forward worklist computing a delta for a NON-seed (Mid)
	// and propagating it field-granularly.
	function testHeaderInvalidationTransitive() {
		vfs.putContent("Leaf.hx", 'class Leaf { public static var X = true; }');
		vfs.putContent("Mid.hx", 'class Mid {
	public static function getX() return Leaf.X;
	public static function other(n:Int):Int return n;
}');
		vfs.putContent("ConsumerA.hx", 'class ConsumerA { public static function use() return Mid.getX(); }');
		vfs.putContent("ConsumerB.hx", 'class ConsumerB { public static function use():Int return Mid.other(1); }');
		vfs.putContent("Main.hx", 'class Main { static function main() { ConsumerA.use(); ConsumerB.use(); } }');
		var args = ["-main", "Main", "-js", "no.js", "--no-output", "-D", "hxb.header_invalidation"];
		runHaxe(args);
		assertSuccess();

		// Signature change on the leaf (Bool -> Int). Mid.getX's return type changes; ConsumerA observes
		// it (re-typed) while ConsumerB uses only Mid.other (header-stable) and must be spared.
		vfs.putContent("Leaf.hx", 'class Leaf { public static var X = 42; }');
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Leaf.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("ConsumerB");
		Assert.isFalse(hasMessage("reusing Mid"));
		Assert.isFalse(hasMessage("reusing ConsumerA"));
	}

	function twoFn(retA:String, retB:String) {
		return 'class Dep {
	public static function a(n:Int):$retA return n;
	public static function b(n:Int):$retB return n;
}';
	}

	function dep(ret:String, body:String) {
		return 'class Dep {
	public var v:Int;
	public function new() v = 0;
	public function bump(n:Int):$ret { return $body; }
}';
	}

	// Parses "header changed N" from the last [measure-signatures] line of the current request.
	function measureChangedCount():Int {
		var re = ~/header changed (\d+)/;
		var result = -1;
		for (m in messages)
			if (re.match(m)) result = Std.parseInt(re.matched(1));
		return result;
	}

	function depContent() {
		return "class Dep {
	public var v:Int;
	public function new() v = 0;
	public inline function bump(n:Int):Int { var t = v + n; return t * 2; }
	public static inline var K = 3;
}";
	}

	function mainContent(tag:String) {
		return 'class Main {
	static function main() {
		$tag
		var d = new Dep();
		trace(d.bump(Dep.K));
	}
}';
	}

	function depSignature() {
		return sys.io.File.getContent(Path.join([testDir, "dump", "AfterInlining", "js", "Dep.dump"]));
	}
#end
}
