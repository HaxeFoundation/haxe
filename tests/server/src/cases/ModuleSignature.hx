package cases;

import haxe.io.Path;
import TestCase;
import utest.Assert;

class ModuleSignature extends TestCase {
	// The signature dumped for a module restored from the hxb cache must be byte-identical to the one
	// dumped when the module was freshly typed. This is the determinism precondition for using module
	// signatures as the module-header diff layer: in particular, the var ids the hxb reader reassigns
	// to a restored module's expressions must normalize away (impl-field bodies are rendered with
	// positional var tokens), and no compiler-stage-derived meta may leak in.
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
		runHaxe(args);
		assertSuccess();
		assertReuse("Dep");
		var restored = depSignature();

		Assert.equals(fresh, restored);
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
