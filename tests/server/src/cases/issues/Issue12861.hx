package cases.issues;

import haxe.display.Diagnostic;

// Regression test for https://github.com/HaxeFoundation/haxe/issues/12861
// Stale diagnostics must be cleared after the underlying problem is fixed.
class Issue12861 extends TestCase {
	function test(_) {
		// File with a type error: Int has no field charAt
		final errContent = getTemplate("diagnostics/Issue12861.hx");
		// Fixed version: use String instead of Int
		final okContent = "class Issue12861 { static function main() { var v:String = ''; var _ = v.charAt(0); } }";

		final args = ["--main", "Issue12861", "--interp", "--no-output"];

		// --- First diagnostics run: file has the type error ---
		vfs.putContent("Issue12861.hx", errContent);
		final res1 = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Issue12861.hx")});
		Assert.equals(1, res1.length);
		final diags1:Array<Diagnostic<Dynamic>> = cast res1[0].diagnostics;
		// Exactly one MissingFields diagnostic expected (Int has no field charAt)
		Assert.equals(1, diags1.length);
		Assert.isTrue(diags1.exists(d -> d.kind == MissingFields));

		// --- Second diagnostics run on the SAME erroneous file (after invalidation) ---
		// Without the fix, add_module_diagnostic polluted Int's cached
		// m_cache_bound_objects on the first run.  The second run would then
		// replay that stale message AND add another one, producing 2 entries.
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Issue12861.hx")});
		final res2 = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Issue12861.hx")});
		Assert.equals(1, res2.length);
		final diags2:Array<Diagnostic<Dynamic>> = cast res2[0].diagnostics;
		// Must still be exactly 1, not 2 or more (no stacking)
		Assert.equals(1, diags2.length);
		Assert.isTrue(diags2.exists(d -> d.kind == MissingFields));

		// --- Third diagnostics run: fix the file ---
		// The old error must not persist; the result must be clean.
		vfs.putContent("Issue12861.hx", okContent);
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Issue12861.hx")});
		final res3 = runHaxeJson(args, DisplayMethods.Diagnostics, {file: new FsPath("Issue12861.hx")});
		// Without the fix, the stale DKMissingFields from Int's cache was
		// replayed here, making res3 still show an error.
		if (res3.length > 0) {
			final diags3:Array<Diagnostic<Dynamic>> = cast res3[0].diagnostics;
			Assert.isFalse(diags3.exists(d -> d.kind == MissingFields));
		}
	}
}
