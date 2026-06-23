package cases;

import haxe.display.FsPath;
import haxe.display.Server;
import haxe.io.Path;
import TestCase;
import utest.Assert;

class ModuleSignature extends TestCase {
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
}
