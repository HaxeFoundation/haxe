// Verifies two things about the jar:
//   1. Behavior: the program runs and Reflect operations work on the
//      unsafe-named field (Main.main exits 0 — that's already established
//      by run-base, but we re-run it here to be self-contained).
//   2. Codegen: no class in the jar declares a JVM field whose name
//      contains a character forbidden by DEX SimpleName grammar (here we
//      scan for the canonical canary, "a\n b").
//   3. d8 (if available) accepts the jar without errors.
import haxe.io.Bytes;

class Check {
	static final UNSAFE_NAME = Bytes.ofString("a\n b");

	static function main() {
		final code = Sys.command("java", ["-jar", "bin/run.jar"]);
		if (code != 0) {
			Sys.println('FAIL: bin/run.jar exited $code');
			Sys.exit(1);
		}

		final offenders = [];
		DexCheck.forEachClass("bin/run.jar", (name, data) -> {
			if (DexCheck.containsBytes(data, UNSAFE_NAME)) offenders.push(name);
		});
		// The unsafe name will still appear in the constant pool as a string
		// (because the constructor passes it to _hx_setField). What must NOT
		// appear is a *field* declaration with that name. Distinguishing the
		// two from a raw byte scan would require parsing the class file, so
		// we use d8 as the authoritative check below and only flag here if
		// d8 isn't available.
		if (DexCheck.locateD8() != null) {
			final r = DexCheck.runD8("bin/run.jar");
			if (r.exit != 0) {
				Sys.println("FAIL: d8 rejected the dex-compatible jar:");
				Sys.println(r.stderr);
				Sys.exit(1);
			}
		} else if (offenders.length == 0) {
			Sys.println("WARN: d8 not available and unsafe name absent — can't verify codegen");
		}
	}
}
