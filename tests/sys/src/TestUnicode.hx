import utest.Assert;
import haxe.io.Bytes;
import haxe.io.Path;
import sys.FileSystem;
import sys.io.File;
import UnicodeSequences.UnicodeString;
import UnicodeSequences.codepointsToString;
import UnicodeSequences.showUnicodeString;
import UtilityProcess.runUtility;

class TestUnicode extends utest.Test {
	static var BIN_SYMLINK =
#if cpp
		#if debug
			"bin-cpp-debug";
		#else
			"bin-cpp";
		#end
#elseif cs
		#if debug
			"bin-cs-debug";
		#else
			"bin-cs";
		#end
#elseif hl
		"bin-hl";
#elseif lua
		"bin-lua";
#elseif jvm
		"bin-jvm";
#elseif java
		#if debug
			"bin-java-debug";
		#else
			"bin-java";
		#end
#elseif neko
		"bin-neko";
#elseif php
		"bin-php";
#elseif python
		"bin-py";
#elseif eval
		"bin-eval";
#elseif js
		"bin-js";
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
	static var names:Array<UnicodeString> = UnicodeSequences.validFilenames;

	// extra files only present in the root test-res directory
	static var namesRoot = names.concat([
		Only([0x0061]), // a
		Only([0x0062]), // b
		Only([0x64, 0x61, 0x74, 0x61, 0x2E, 0x62, 0x69, 0x6E]) // data.bin
	]);

	static var endLine = (Sys.systemName() == "Windows" ? "\r\n" : "\n");

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

	function pathBoth(f:String->Void, ?path:String, ?skipNonExistent:Bool = true):Void {
		for (filename in names) switch (filename) {
			case Only(codepointsToString(_) => ref):
			f(path != null ? Path.join([path, ref]) : ref);
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			var joinedNfc = path != null ? Path.join([path, nfc]) : nfc;
			var joinedNfd = path != null ? Path.join([path, nfd]) : nfd;
			if (!skipNonExistent || FileSystem.exists(joinedNfc)) f(joinedNfc);
			if (!skipNonExistent || FileSystem.exists(joinedNfd)) f(joinedNfd);
		}
	}

	function assertNormalEither(f:String->Bool, path:String, ?msg:String, ?pos:haxe.PosInfos):Void {
		for (filename in names) Assert.isTrue(switch (filename) {
			case Only(codepointsToString(_) => ref): f(Path.join([path, ref]));
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			f(Path.join([path, nfc])) || f(Path.join([path, nfd]));
		}, '$msg ($filename in $path)', pos);
	}

	function setupClass() {
		FileSystem.createDirectory("temp-unicode");
		#if TEST_INVALID_UNICODE_FS
		Sys.command("python3", ["gen_test_res.py", "TEST_INVALID_UNICODE_FS"]);
		#else
		Sys.command("python3", ["gen_test_res.py"]);
		#end
	}

	function teardownClass() {
		if (FileSystem.exists("temp-unicode")) {
			for (file in FileSystem.readDirectory("temp-unicode")) {
				FileSystem.deleteFile(Path.join(["temp-unicode", file]));
			}
			FileSystem.deleteDirectory("temp-unicode");
		}
	}

#if target.unicode
	function testFilesystem() {
#if !java // java does not have this functionality
#if !cs // C# disabled temporarily (#8247)
		// setCwd + getCwd
		Sys.setCwd("test-res");
		function enterLeave(dir:String, ?alt:String):Void {
			Sys.setCwd(dir);
			assertUEnds(Path.removeTrailingSlashes(Path.normalize(Sys.getCwd())), '/test-res/${dir}', alt != null ? '/test-res/${alt}' : null);
			Sys.setCwd("..");
		}
		for (filename in names) switch (filename) {
			case Only(codepointsToString(_) => ref): enterLeave(ref);
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			if (FileSystem.exists(nfc)) enterLeave(nfc, nfd);
			if (FileSystem.exists(nfd)) enterLeave(nfd, nfc);
		}
		Sys.setCwd("..");
#end
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
						Path.normalize(FileSystem.absolutePath('$path/${relative.path}')),
						relative.end
					);
			}, "test-res");

#if !java // java does not have this functionality
#if !cs // C# disabled temporarily (#8247)
		assertNormalEither(path -> {
				if (!FileSystem.exists(path)) return false; // NFC/NFD differences
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
					]) if (!StringTools.endsWith(Path.normalize(FileSystem.absolutePath('${relative.path}')), relative.end)) ret = false;
				Sys.setCwd("../..");
				return ret;
			}, "test-res", "setCwd + absolutePath + endsWith failed");
#end
#end

		// exists
		assertNormalEither(FileSystem.exists, 'test-res/a', 'expected exists == true');
		assertNormalEither(FileSystem.exists, 'test-res/b', 'expected exists == false');

		// fullPath
#if !lua // Lua disabled temporarily (#8215)
		#if !cs // C# behaves like Windows here
		if (Sys.systemName() != "Windows") {
			// symlinks behave strangely on Windows
			pathBoth(path -> {
					assertUEnds(
							Path.normalize(FileSystem.fullPath('$path/${BIN_SYMLINK}')),
							'/${UtilityProcess.BIN_PATH}/${UtilityProcess.BIN_NAME}'
						);
				}, "test-res");
		}
		#end
#end

		// isDirectory
		assertNormalEither(FileSystem.isDirectory, 'test-res/a', 'expected isDirectory == true');
		assertNormalEither(path -> !FileSystem.isDirectory(path), 'test-res/b', 'expected isDirectory == false');

		// readDirectory
#if !cs // C# disabled temporarily (#8247)
		sameFiles(FileSystem.readDirectory("test-res"), namesRoot);
		sameFiles(FileSystem.readDirectory("test-res/a"), names);
		sameFiles(FileSystem.readDirectory("test-res/b"), names);
#end

		// stat
		assertNormalEither(path -> FileSystem.stat(path) != null, 'test-res/a', 'expected stat != null');
		assertNormalEither(path -> FileSystem.stat(path) != null, 'test-res/b', 'expected stat != null');

		// path
		pathBoth(str -> {
				Assert.equals(new Path('$str/a.b').dir, str);
				Assert.equals(Path.directory('$str/a.b'), str);
				Assert.equals(new Path('a/$str.b').file, str);
				Assert.equals(new Path('a/b.$str').ext, str);
				Assert.equals(Path.extension('a/b.$str'), str);
				Assert.equals(Path.join([str, "a"]), '$str/a');
				Assert.equals(Path.join(["a", str]), 'a/$str');
				Assert.equals(Path.addTrailingSlash(str), '$str/');
				Assert.equals(Path.normalize('a/../$str'), str);
				Assert.equals(Path.normalize('$str/a/..'), str);
			});

		// rename
		File.copy("test-res/data.bin", "temp-unicode/rename-me");
		pathBoth(str -> {
				FileSystem.rename('temp-unicode/rename-me', 'temp-unicode/$str');
				Assert.isFalse(FileSystem.exists('temp-unicode/rename-me'));
				Assert.isTrue(FileSystem.exists('temp-unicode/$str'));
				FileSystem.rename('temp-unicode/$str', 'temp-unicode/rename-me');
			});

		pathBoth(str -> {
				// copy
				File.copy("test-res/data.bin", 'temp-unicode/$str');
				Assert.isTrue(FileSystem.exists('temp-unicode/$str'));
				assertBytesEqual(File.getBytes('temp-unicode/$str'), UnicodeSequences.validBytes);

				// deleteFile
				FileSystem.deleteFile('temp-unicode/$str');
				Assert.isFalse(FileSystem.exists('temp-unicode/$str'));

				// createDirectory
				FileSystem.createDirectory('temp-unicode/$str');
				Assert.isTrue(FileSystem.exists('temp-unicode/$str'));
				Assert.equals(FileSystem.readDirectory('temp-unicode/$str').length, 0);

				// deleteDirectory
				FileSystem.deleteDirectory('temp-unicode/$str');
				Assert.isFalse(FileSystem.exists('temp-unicode/$str'));
			});
	}

	// Temporary disabled for local run because of https://github.com/HaxeFoundation/haxe/issues/8380
	#if github
	function testIPC() {
		// stdin.readLine
		UnicodeSequences.normalBoth(str -> {
				assertUEquals(runUtility(["stdin.readLine"], {stdin: str + endLine}).stdout, str + endLine);
			});

		// stdin.readString
		UnicodeSequences.normalBoth(str -> {
				var byteLength = Bytes.ofString(str).length;
				assertUEquals(runUtility(["stdin.readString", '${byteLength}'], {stdin: '$str'}).stdout, str + endLine);
			});

		// stdin.readUntil
		UnicodeSequences.normalBoth(str -> {
				// make sure the 0x70 byte is not part of the test string
				assertUEquals(runUtility(["stdin.readUntil", "0x70"], {stdin: str + "\x70" + str + "\x70"}).stdout, str + endLine);
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
				assertUEquals(runUtility(["println", '$i', mode]).stdout, str + endLine);
				// trace
				assertUEnds(runUtility(["trace", '$i', mode]).stdout, str + endLine);
				#if !java
				// putEnv + getEnv
				assertUEquals(runUtility(["putEnv", "HAXE_TEST", '$i', mode, "getEnv", "HAXE_TEST"]).stdout, str + endLine);
				// putEnv + environment
				assertUEquals(runUtility(["putEnv", "HAXE_TEST", '$i', mode, "environment", "HAXE_TEST"]).stdout, str + endLine);
				#end
			});

		// args
		#if !cs // C# behaves like Windows here
		if (#if (java || eval || cpp) Sys.systemName() != "Windows" #else true #end) {
			// https://stackoverflow.com/questions/7660651/passing-command-line-unicode-argument-to-java-code
			UnicodeSequences.normalBoth(str -> {
					assertUEquals(runUtility(["args", str]).stdout, str + endLine);
				});
		}
		#end
	}
	#end

	function testIO() {
		// getBytes
		assertBytesEqual(File.getBytes("test-res/data.bin"), UnicodeSequences.validBytes);
		pathBoth(path -> {
				assertBytesEqual(File.getBytes(path), UnicodeSequences.validBytes);
			}, "test-res/b");

		// getContent
		assertUEquals(File.getContent("test-res/data.bin"), UnicodeSequences.validString);
		pathBoth(path -> {
				assertUEquals(File.getContent(path), UnicodeSequences.validString);
			}, "test-res/b");

		// saveContent
		File.saveContent("temp-unicode/data.bin", UnicodeSequences.validString);
		assertBytesEqual(File.getBytes("temp-unicode/data.bin"), UnicodeSequences.validBytes);
#if !cs // C# disabled temporarily (#8247)
		pathBoth(str -> {
				File.saveContent('temp-unicode/saveContent-$str.bin', UnicodeSequences.validString);
				assertBytesEqual(File.getBytes('temp-unicode/saveContent-$str.bin'), UnicodeSequences.validBytes);
			});
#end

		// write
		var out = File.write("temp-unicode/out.bin");
		out.writeString(UnicodeSequences.validString);
		out.close();
		assertBytesEqual(File.getBytes("temp-unicode/out.bin"), UnicodeSequences.validBytes);
#if !cs // C# disabled temporarily (#8247)
		pathBoth(str -> {
				var out = File.write('temp-unicode/write-$str.bin');
				out.writeString(UnicodeSequences.validString);
				out.close();
				assertBytesEqual(File.getBytes('temp-unicode/write-$str.bin'), UnicodeSequences.validBytes);
			});
#end

		// update
		var out = File.update("temp-unicode/out.bin");
		out.seek(0, SeekBegin);
		out.writeString(UnicodeSequences.validString);
		out.close();
		assertBytesEqual(File.getBytes("temp-unicode/out.bin"), UnicodeSequences.validBytes);

		// append
#if js
		if (Sys.systemName() != "Mac") // File.append() here is broken on mac
#end
		{
			var out = File.append("temp-unicode/out.bin");
			out.writeString(UnicodeSequences.validString);
			out.close();
			var repeated = Bytes.alloc(UnicodeSequences.validBytes.length * 2);
			repeated.blit(0, UnicodeSequences.validBytes, 0, UnicodeSequences.validBytes.length);
			repeated.blit(UnicodeSequences.validBytes.length, UnicodeSequences.validBytes, 0, UnicodeSequences.validBytes.length);
			assertBytesEqual(File.getBytes("temp-unicode/out.bin"), repeated);
		}

		// readLine
		var data = File.read("test-res/data.bin");
		UnicodeSequences.normalNFC(str -> {
				var line = data.readLine();
				assertUEquals(line, str);
			});

		// readString
		data.seek(0, SeekBegin);
		UnicodeSequences.normalNFC(str -> {
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
