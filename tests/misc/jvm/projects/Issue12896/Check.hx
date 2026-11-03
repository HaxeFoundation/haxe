// Looks for byte triples `;X>` or `;X;` where X is one of the eight JVM
// primitive descriptor letters (BCDFIJSZ). These cannot occur in any valid
// class-file attribute or constant-pool string — see JVMS 4.7.9.1: type
// arguments inside `<...>` must be reference types, and concatenated
// signature lists outside `<...>` always have `L`/`[`/`T` after `;`.
class Check {
	static final PRIMS = "BCDFIJSZ";

	static function main() {
		// d8 emits a *warning* for this (not an error) and still exits 0,
		// so check stderr rather than exit code.
		if (DexCheck.locateD8() != null) {
			final r = DexCheck.runD8("bin/run.jar");
			if (r.stderr.indexOf("Invalid signature") >= 0) {
				Sys.println("FAIL: d8 reported invalid signature:");
				Sys.println(r.stderr);
				Sys.exit(1);
			}
		}

		final offenders = [];
		DexCheck.forEachClass("bin/run.jar", (name, data) -> {
			if (hasPrimitiveTypeArg(data)) offenders.push(name);
		});

		if (offenders.length > 0) {
			Sys.println("FAIL: --jvm emitted a Signature attribute with a primitive as a generic type argument in:");
			for (n in offenders) Sys.println("  " + n);
			Sys.println("d8/r8 will reject these signatures. genjvm should box primitives in type-argument position.");
			Sys.exit(1);
		}
	}

	static function hasPrimitiveTypeArg(data:haxe.io.Bytes):Bool {
		var i = 0;
		while (i < data.length - 2) {
			if (data.get(i) == ";".code) {
				final mid = data.get(i + 1);
				final end = data.get(i + 2);
				if (PRIMS.indexOf(String.fromCharCode(mid)) >= 0
					&& (end == ">".code || end == ";".code)) {
					return true;
				}
			}
			i++;
		}
		return false;
	}
}
