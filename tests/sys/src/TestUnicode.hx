import utest.Assert;
import haxe.io.Bytes;
import UnicodeSequences.UnicodeString;
import UnicodeSequences.codepointsToString;
import UnicodeSequences.showUnicodeString;
import UtilityProcess.runUtility;

class TestUnicode extends utest.Test {
	static var BIN_SYMLINK =
#if cpp
		"bin-cpp";
#elseif cs
		"bin-cs";
#elseif hl
		"bin-hl";
#elseif java
		"bin-java";
#elseif neko
		"bin-neko";
#elseif php
		"bin-php";
#elseif python
		"bin-py";
#else
		null;
#end

	// list of filenames expected to NOT exist in sub-directories
	static var nonExistentNames:Array<UnicodeString> = [
		// Java escapes
		Only([0x0025, 0x0030 , 0x0001]), // %01
		Only([0x0025, 0x0037 , 0x0046]) // %7F
	];

	// list of expected filenames in sub-directories
	static var names:Array<UnicodeString> = UnicodeSequences.valid;

	// extra files only present in the root test-res directory
	static var namesRoot = names.concat([
		Only([0x0061]), // a
		Only([0x0062]), // b
		Only([0x64, 0x61, 0x74, 0x61, 0x2E, 0x62, 0x69, 0x6E]) // data.bin
	]);

	// same names and length, but possibly different order
	// assumes no duplicates in expected
	function sameFiles(actual:Array<String>, expected:Array<UnicodeString>):Void {
		Assert.equals(actual.length, expected.length);
		var remaining = expected.copy();
		for (file in actual) {
			var codepoints = UnicodeSequences.unicodeCodepoints(file);
			var removed = remaining.filter(ref -> !UnicodeSequences.codepointsSame(codepoints, ref));
			if (removed.length == remaining.length) {
				Assert.fail('unexpected filename ${showUnicodeString(file)} found');
			} else {
				remaining = removed;
			}
		}
	}

	function assertUEnds(actual:String, expected:String, ?alt:String, ?pos:haxe.PosInfos):Void {
		Assert.isTrue(
			StringTools.endsWith(actual, expected) || (alt != null ? StringTools.endsWith(actual, alt) : false),
			'expected ${showUnicodeString(actual)} to end with ${showUnicodeString(expected)}'
			+ (alt != null ? ' or ${showUnicodeString(alt)}' : ""),
			pos
		);
	}

	function assertUEquals(actual:String, expected:String, ?msg:String, ?pos:haxe.PosInfos):Void {
		Assert.equals(
			expected, actual,
			msg != null ? msg : 'expected ${showUnicodeString(actual)} to be ${showUnicodeString(expected)}',
			pos
		);
	}

	function assertBytesEqual(actual:Bytes, expected:Bytes, ?msg:String, ?pos:haxe.PosInfos):Void {
		Assert.equals(
			actual.compare(expected), 0,
			msg != null ? msg : 'expected ${actual.toHex()} to be ${expected.toHex()}',
			pos
		);
	}

	function pathBoth(f:String->Void, path:String, ?skipNonExistent:Bool = true):Void {
		for (filename in names) switch (filename) {
			case Only(codepointsToString(_) => ref):
			f('$path/$ref');
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			if (sys.FileSystem.exists('$path/$nfc') || !skipNonExistent) f('$path/$nfc');
			if (sys.FileSystem.exists('$path/$nfd') || !skipNonExistent) f('$path/$nfd');
		}
	}

	function assertNormalEither(f:String->Bool, path:String, ?msg:String, ?pos:haxe.PosInfos):Void {
		for (filename in names) Assert.isTrue(switch (filename) {
			case Only(codepointsToString(_) => ref): f('$path/$ref');
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			f('$path/$nfc') || f('$path/$nfd');
		}, '$msg ($filename in $path)', pos);
	}

#if (target.unicode)
	function testFilesystem() {
#if !(java)
		// setCwd + getCwd
		Sys.setCwd("test-res");
		function enterLeave(dir:String, ?alt:String):Void {
			Sys.setCwd(dir);
			assertUEnds(Sys.getCwd(), '/test-res/${dir}', alt != null ? '/test-res/${alt}' : null);
			Sys.setCwd("..");
		}
		for (filename in names) switch (filename) {
			case Only(codepointsToString(_) => ref): enterLeave(ref);
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			if (sys.FileSystem.exists(nfc)) enterLeave(nfc, nfd);
			if (sys.FileSystem.exists(nfd)) enterLeave(nfd, nfc);
		}
		Sys.setCwd("..");
#end

		// programPath
#if !(java || python) // these targets resolve symlinked files
		pathBoth(path -> {
				assertUEnds(runUtility(["programPath"], {execPath: path, execName: BIN_SYMLINK}).stdout, '$path/${BIN_SYMLINK}\n');
			}, "test-res");
#end

		// absolutePath
		pathBoth(path -> {
				for (relative in [
						{path: '../$path', end: '${path}'},
						{path: 'foo', end: '${path}/foo'},
						{path: "..", end: "test-res"},
						{path: "./././././", end: path},
						{path: "./..", end: "test-res"},
						{path: "./čýžé", end: '${path}/čýžé'},
						{path: "./čýžé/", end: '${path}/čýžé'},
						{path: "./../čýžé", end: 'test-res/čýžé'},
						{path: "./../čýžé/", end: 'test-res/čýžé'},
					]) assertUEnds(
						sys.FileSystem.absolutePath('$path/${relative.path}'),
						relative.end
					);
			}, "test-res");

#if !(java)
		assertNormalEither(path -> {
				if (!sys.FileSystem.exists(path)) return false; // NFC/NFD differences
				Sys.setCwd(path);
				var ret = true;
				for (relative in [
						{path: '../$path', end: '${path}'},
						{path: 'foo', end: '${path}/foo'},
						{path: "..", end: "test-res"},
						{path: "./././././", end: path},
						{path: "./..", end: "test-res"},
						{path: "./čýžé", end: '${path}/čýžé'},
						{path: "./čýžé/", end: '${path}/čýžé'},
						{path: "./../čýžé", end: 'test-res/čýžé'},
						{path: "./../čýžé/", end: 'test-res/čýžé'},
					]) if (!StringTools.endsWith(sys.FileSystem.absolutePath('${relative.path}'), relative.end)) ret = false;
				Sys.setCwd("../..");
				return ret;
			}, "test-res", "setCwd + absolutePath + endsWith failed");
#end

		// exists
		assertNormalEither(sys.FileSystem.exists, 'test-res/a', 'expected exists == true');
		assertNormalEither(sys.FileSystem.exists, 'test-res/b', 'expected exists == false');

		// fullPath
		pathBoth(path -> {
				assertUEnds(
						sys.FileSystem.fullPath('$path/${BIN_SYMLINK}'),
						'/${UtilityProcess.BIN_PATH}/${UtilityProcess.BIN_NAME}'
					);
			}, "test-res");

		// isDirectory
		assertNormalEither(sys.FileSystem.isDirectory, 'test-res/a', 'expected isDirectory == true');
		assertNormalEither(path -> !sys.FileSystem.isDirectory(path), 'test-res/b', 'expected isDirectory == false');

		// readDirectory
		sameFiles(sys.FileSystem.readDirectory("test-res"), namesRoot);
		sameFiles(sys.FileSystem.readDirectory("test-res/a"), names);
		sameFiles(sys.FileSystem.readDirectory("test-res/b"), names);

		// stat
		assertNormalEither(path -> sys.FileSystem.stat(path) != null, 'test-res/a', 'expected stat != null');
		assertNormalEither(path -> sys.FileSystem.stat(path) != null, 'test-res/b', 'expected stat != null');

		// path
		UnicodeSequences.normalBoth(str -> {
				Assert.equals(new haxe.io.Path('$str/a.b').dir, str);
				Assert.equals(haxe.io.Path.directory('$str/a.b'), str);
				Assert.equals(new haxe.io.Path('a/$str.b').file, str);
				Assert.equals(new haxe.io.Path('a/b.$str').ext, str);
				Assert.equals(haxe.io.Path.extension('a/b.$str'), str);
				Assert.equals(haxe.io.Path.join([str, "a"]), '$str/a');
				Assert.equals(haxe.io.Path.join(["a", str]), 'a/$str');
				Assert.equals(haxe.io.Path.addTrailingSlash(str), '$str/');
				Assert.equals(haxe.io.Path.normalize('a/../$str'), str);
				Assert.equals(haxe.io.Path.normalize('$str/a/..'), str);
			});

		// rename
		sys.io.File.copy("test-res/data.bin", "temp-unicode/rename-me");
		UnicodeSequences.normalBoth(str -> {
				sys.FileSystem.rename('temp-unicode/rename-me', 'temp-unicode/$str');
				Assert.isFalse(sys.FileSystem.exists('temp-unicode/rename-me'));
				Assert.isTrue(sys.FileSystem.exists('temp-unicode/$str'));
				sys.FileSystem.rename('temp-unicode/$str', 'temp-unicode/rename-me');
			});

		UnicodeSequences.normalBoth(str -> {
				// copy
				sys.io.File.copy("test-res/data.bin", 'temp-unicode/$str');
				Assert.isTrue(sys.FileSystem.exists('temp-unicode/$str'));
				assertBytesEqual(sys.io.File.getBytes('temp-unicode/$str'), UnicodeSequences.validBytes);

				// deleteFile
				sys.FileSystem.deleteFile('temp-unicode/$str');
				Assert.isFalse(sys.FileSystem.exists('temp-unicode/$str'));

				// createDirectory
				sys.FileSystem.createDirectory('temp-unicode/$str');
				Assert.isTrue(sys.FileSystem.exists('temp-unicode/$str'));
				Assert.equals(sys.FileSystem.readDirectory('temp-unicode/$str').length, 0);

				// deleteDirectory
				sys.FileSystem.deleteDirectory('temp-unicode/$str');
				Assert.isFalse(sys.FileSystem.exists('temp-unicode/$str'));
			});
	}

	function testIPC() {
		// stdin.readLine
		UnicodeSequences.normalBoth(str -> {
				assertUEquals(runUtility(["stdin.readLine"], {stdin: '$str\n'}).stdout, '$str\n');
			});

		// stdin.readString
		UnicodeSequences.normalBoth(str -> {
				// FIXME: readString of UTF8 should maybe read n characters, not bytes? #8199
				var byteLength = Bytes.ofString(str).length;
				assertUEquals(runUtility(["stdin.readString", '${byteLength}'], {stdin: '$str'}).stdout, '$str\n');
			});

		// stdin.readUntil
		UnicodeSequences.normalBoth(str -> {
				// make sure the 0x70 byte is not part of the test string 
				assertUEquals(runUtility(["stdin.readUntil", "0x70"], {stdin: str + "\x70" + str + "\x70"}).stdout, '$str\n');
			});

		UnicodeSequences.normalBothIndexed((str, i, nfc) -> {
				var mode = nfc ? "nfc" : "nfd";
				// stdout
				assertUEquals(runUtility(["stdout.writeString", '$i', mode]).stdout, str);
				// stderr
				assertUEquals(runUtility(["stderr.writeString", '$i', mode]).stderr, str);
				// print
				assertUEquals(runUtility(["print", '$i', mode]).stdout, str);
				// println
				assertUEquals(runUtility(["println", '$i', mode]).stdout, '$str\n');
				// trace
				assertUEnds(runUtility(["trace", '$i', mode]).stdout, '$str\n');
#if !(java)
				// putEnv + getEnv
				assertUEquals(runUtility(["putEnv", "HAXE_TEST", '$i', mode, "getEnv", "HAXE_TEST"]).stdout, '$str\n');
				// putEnv + environment
				assertUEquals(runUtility(["putEnv", "HAXE_TEST", '$i', mode, "environment", "HAXE_TEST"]).stdout, '$str\n');
#end
			});

		// args
		UnicodeSequences.normalBoth(str -> {
				assertUEquals(runUtility(["args", str]).stdout, '$str\n');
			});
	}

	function setupClass() {
		sys.FileSystem.createDirectory("temp-unicode");
	}

	function teardownClass() {
		for (file in sys.FileSystem.readDirectory("temp-unicode")) {
			sys.FileSystem.deleteFile('temp-unicode/$file');
		}
		sys.FileSystem.deleteDirectory("temp-unicode");
	}

	function testIO() {
		// getBytes
		assertBytesEqual(sys.io.File.getBytes("test-res/data.bin"), UnicodeSequences.validBytes);

		// getContent
		assertUEquals(sys.io.File.getContent("test-res/data.bin"), UnicodeSequences.validString);

		// saveContent
		sys.io.File.saveContent("temp-unicode/data.bin", UnicodeSequences.validString);
		assertBytesEqual(sys.io.File.getBytes("temp-unicode/data.bin"), UnicodeSequences.validBytes);

		// write
		var out = sys.io.File.write("temp-unicode/out.bin");
		out.writeString(UnicodeSequences.validString);
		out.close();
		assertBytesEqual(sys.io.File.getBytes("temp-unicode/out.bin"), UnicodeSequences.validBytes);

		// update
		var out = sys.io.File.update("temp-unicode/out.bin");
		out.seek(0, SeekBegin);
		out.writeString(UnicodeSequences.validString);
		out.close();
		assertBytesEqual(sys.io.File.getBytes("temp-unicode/out.bin"), UnicodeSequences.validBytes);

		// append
		var out = sys.io.File.append("temp-unicode/out.bin");
		out.writeString(UnicodeSequences.validString);
		out.close();
		var repeated = Bytes.alloc(UnicodeSequences.validBytes.length * 2);
		repeated.blit(0, UnicodeSequences.validBytes, 0, UnicodeSequences.validBytes.length);
		repeated.blit(UnicodeSequences.validBytes.length, UnicodeSequences.validBytes, 0, UnicodeSequences.validBytes.length);
		assertBytesEqual(sys.io.File.getBytes("temp-unicode/out.bin"), repeated);

		// readLine
		var data = sys.io.File.read("test-res/data.bin");
		UnicodeSequences.normalNFC(str -> {
				var line = data.readLine();
				assertUEquals(line, str);
			});

		// readString
		data.seek(0, SeekBegin);
		UnicodeSequences.normalNFC(str -> {
				// FIXME: readString of UTF8 should read n characters, not bytes? #8199
				var byteLength = Bytes.ofString(str).length;
				var line = data.readString(byteLength + 1); // + newline character
				assertUEquals(line, '$str\n');
			});

		// readUntil
		data.seek(0, SeekBegin);
		UnicodeSequences.normalNFC(str -> {
				var line = data.readUntil(0x0A);
				assertUEquals(line, str);
			});
	}
#end
}
