import haxe.io.Bytes;
import haxe.io.BytesInput;
import haxe.zip.Reader;
import sys.FileSystem;
import sys.io.File;

class Check {
	static final BACKTICK_THIS = Bytes.ofString("`this");

	static function main() {
		final d8 = locateD8();
		if (d8 != null) {
			FileSystem.createDirectory("bin/dex-out");
			final code = Sys.command(d8, ["--min-api", "21", "--output", "bin/dex-out", "bin/run.jar"]);
			if (code != 0) {
				Sys.println("FAIL: d8 rejected the jar (exit " + code + ")");
				Sys.exit(1);
			}
		}

		final entries = Reader.readZip(new BytesInput(File.getBytes("bin/run.jar")));
		final offenders = [];
		for (e in entries) {
			if (!StringTools.endsWith(e.fileName, ".class")) continue;
			final data = Reader.unzip(e);
			if (indexOfBytes(data, BACKTICK_THIS) >= 0) offenders.push(e.fileName);
		}

		if (offenders.length > 0) {
			Sys.println("FAIL: --jvm emitted a synthetic field named `this on:");
			for (n in offenders) Sys.println("  " + n);
			Sys.println("d8/r8 rejects this; genjvm should pick a dex-safe name (e.g. _hx_this).");
			Sys.exit(1);
		}
	}

	static function locateD8():Null<String> {
		final explicit = Sys.getEnv("D8");
		if (explicit != null) return FileSystem.exists(explicit) ? explicit : null;

		// Try ANDROID_SDK first, then the standard SDK env vars exposed by
		// the GitHub-hosted runners and android-actions/setup-android.
		for (envName in ["ANDROID_SDK", "ANDROID_SDK_ROOT", "ANDROID_HOME"]) {
			final sdk = Sys.getEnv(envName);
			if (sdk == null) continue;

			final buildTools = '$sdk/build-tools';
			if (!FileSystem.exists(buildTools) || !FileSystem.isDirectory(buildTools)) continue;

			// Pick the highest-versioned build-tools directory that ships d8.
			final versions = FileSystem.readDirectory(buildTools);
			versions.sort((a, b) -> Reflect.compare(b, a));
			for (v in versions) {
				final candidate = '$buildTools/$v/d8';
				if (FileSystem.exists(candidate)) return candidate;
			}
		}
		return null;
	}

	static function indexOfBytes(haystack:Bytes, needle:Bytes):Int {
		if (needle.length == 0 || needle.length > haystack.length) return -1;
		final first = needle.get(0);
		var i = 0;
		while (i <= haystack.length - needle.length) {
			if (haystack.get(i) == first) {
				var j = 1;
				while (j < needle.length && haystack.get(i + j) == needle.get(j)) j++;
				if (j == needle.length) return i;
			}
			i++;
		}
		return -1;
	}
}
