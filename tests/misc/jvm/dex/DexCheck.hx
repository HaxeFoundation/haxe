import haxe.io.Bytes;
import haxe.io.BytesInput;
import haxe.zip.Reader;
import sys.FileSystem;
import sys.io.File;
import sys.io.Process;

class DexCheck {
	// Locate the d8 binary. Honors $D8 first, then standard Android SDK env
	// vars; picks the highest-versioned build-tools dir that ships d8.
	public static function locateD8():Null<String> {
		final explicit = Sys.getEnv("D8");
		if (explicit != null) return FileSystem.exists(explicit) ? explicit : null;

		for (envName in ["ANDROID_SDK", "ANDROID_SDK_ROOT", "ANDROID_HOME"]) {
			final sdk = Sys.getEnv(envName);
			if (sdk == null) continue;

			final buildTools = '$sdk/build-tools';
			if (!FileSystem.exists(buildTools) || !FileSystem.isDirectory(buildTools)) continue;

			final versions = FileSystem.readDirectory(buildTools);
			versions.sort((a, b) -> Reflect.compare(b, a));
			for (v in versions) {
				final candidate = '$buildTools/$v/d8';
				if (FileSystem.exists(candidate)) return candidate;
			}
		}
		return null;
	}

	public static function runD8(jar:String, minApi:Int = 21):{exit:Int, stdout:String, stderr:String} {
		final d8 = locateD8();
		if (d8 == null) throw "d8 not found (set $D8 or $ANDROID_SDK_ROOT)";

		final outDir = haxe.io.Path.directory(jar) + "/dex-out";
		if (FileSystem.exists(outDir)) {
			for (f in FileSystem.readDirectory(outDir)) FileSystem.deleteFile('$outDir/$f');
		} else {
			FileSystem.createDirectory(outDir);
		}

		final p = new Process(d8, ["--min-api", Std.string(minApi), "--output", outDir, jar]);
		final stdout = p.stdout.readAll().toString();
		final stderr = p.stderr.readAll().toString();
		final exit = p.exitCode();
		p.close();
		return {exit: exit, stdout: stdout, stderr: stderr};
	}

	public static function forEachClass(jar:String, fn:(name:String, data:Bytes)->Void):Void {
		final entries = Reader.readZip(new BytesInput(File.getBytes(jar)));
		for (e in entries) {
			if (!StringTools.endsWith(e.fileName, ".class")) continue;
			fn(e.fileName, Reader.unzip(e));
		}
	}

	public static function indexOfBytes(haystack:Bytes, needle:Bytes):Int {
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

	public static inline function containsBytes(haystack:Bytes, needle:Bytes):Bool {
		return indexOfBytes(haystack, needle) >= 0;
	}
}
