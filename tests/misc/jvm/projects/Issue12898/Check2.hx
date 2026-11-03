import haxe.io.Bytes;
import haxe.io.BytesInput;
import haxe.zip.Reader;
import sys.io.File;

// Verifies that jvm closure-class merging produced a single jar entry per
// (target_class, jvm_method_name) — no duplicate Buf$..._add.class — and
// that d8 (if available) accepts the jar.
class Check2 {
	static function main() {
		final names = new Map<String, Int>();
		final entries = Reader.readZip(new BytesInput(File.getBytes("bin/run2.jar")));
		for (e in entries) {
			if (!StringTools.endsWith(e.fileName, ".class")) continue;
			names.set(e.fileName, (names.exists(e.fileName) ? names.get(e.fileName) : 0) + 1);
		}
		final dups = [for (n => c in names) if (c > 1) '$n ($c)'];
		if (dups.length > 0) {
			Sys.println("FAIL: jar contains duplicate class entries:");
			for (d in dups) Sys.println("  " + d);
			Sys.exit(1);
		}

		final code = Sys.command("java", ["-jar", "bin/run2.jar"]);
		if (code != 0) {
			Sys.println('FAIL: bin/run2.jar exited $code');
			Sys.exit(1);
		}

		if (DexCheck.locateD8() != null) {
			final r = DexCheck.runD8("bin/run2.jar");
			if (r.exit != 0) {
				Sys.println("FAIL: d8 rejected the jar:");
				Sys.println(r.stderr);
				Sys.exit(1);
			}
		}
	}
}
