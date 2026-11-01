import haxe.io.Bytes;

class Check {
	static final BACKTICK_THIS = Bytes.ofString("`this");

	static function main() {
		// d8 rejects the jar outright when a synthetic field is named `this,
		// so a non-zero exit is enough to detect the regression.
		if (DexCheck.locateD8() != null) {
			final r = DexCheck.runD8("bin/run.jar");
			if (r.exit != 0) {
				Sys.println("FAIL: d8 rejected the jar (exit " + r.exit + ")");
				Sys.println(r.stderr);
				Sys.exit(1);
			}
		}

		final offenders = [];
		DexCheck.forEachClass("bin/run.jar", (name, data) -> {
			if (DexCheck.containsBytes(data, BACKTICK_THIS)) offenders.push(name);
		});

		if (offenders.length > 0) {
			Sys.println("FAIL: --jvm emitted a synthetic field named `this on:");
			for (n in offenders) Sys.println("  " + n);
			Sys.println("d8/r8 rejects this; genjvm should pick a dex-safe name (e.g. _hx_this).");
			Sys.exit(1);
		}
	}
}
