import sys.FileSystem;

// Runs d8 against a list of jars (passed as CLI args) and reports any
// errors or non-allowlisted warnings. Exits non-zero on any finding so the
// caller (CI / makefile target) can fail the build.
//
// Allowlist entries are substrings matched anywhere in a warning block. Add
// to BENIGN with a comment explaining *why* the warning is acceptable —
// anything else surfacing here should be triaged into a bug + Issue test.
class VerifyAllJars {
	// d8 emits per-warning blocks separated by blank lines. Each block whose
	// first line contains one of these substrings is dropped from the report.
	static final BENIGN = [
		// `--lib android.jar` would silence this legitimately, but we don't
		// want to require an SDK install for local runs. Affects classes
		// that implement / desugar against framework interfaces (Iterator,
		// BiFunction, Cloneable, …) and is purely a classpath-at-dex-time
		// note, not a codegen problem.
		"is required for default or static interface methods desugaring",
	];

	static function main() {
		final args = Sys.args();
		final softMissingD8 = args.remove("--soft");
		final jars = args.filter(a -> a != "");

		if (jars.length == 0) {
			Sys.println("usage: VerifyAllJars [--soft] <jar> [jar...]");
			Sys.exit(2);
		}

		if (DexCheck.locateD8() == null) {
			final msg = "d8 not found (set $D8 or $ANDROID_SDK_ROOT/build-tools)";
			if (softMissingD8) {
				Sys.println("SKIP: " + msg);
				Sys.exit(0);
			}
			Sys.println("FAIL: " + msg);
			Sys.exit(1);
		}

		var bad = 0;
		for (jar in jars) {
			if (!FileSystem.exists(jar)) {
				Sys.println('SKIP: $jar (not found)');
				continue;
			}
			final r = DexCheck.runD8(jar);
			final issues = filterStderr(r.stderr);
			if (r.exit != 0 || issues.length > 0) {
				Sys.println('--- $jar (exit ${r.exit}, ${issues.length} issue(s)) ---');
				for (block in issues) Sys.println(block);
				bad++;
			} else {
				Sys.println('OK: $jar');
			}
		}

		Sys.println('${jars.length - bad}/${jars.length} jars clean');
		Sys.exit(bad == 0 ? 0 : 1);
	}

	// Split d8 stderr into per-warning blocks, drop allowlisted ones.
	static function filterStderr(stderr:String):Array<String> {
		final out = [];
		final blocks = ~/\r?\n\r?\n/g.split(stderr);
		for (b in blocks) {
			final trimmed = StringTools.trim(b);
			if (trimmed == "") continue;
			var allow = false;
			for (pat in BENIGN) if (trimmed.indexOf(pat) >= 0) { allow = true; break; }
			if (!allow) out.push(trimmed);
		}
		return out;
	}
}
