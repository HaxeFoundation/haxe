import utest.Assert;
import haxe.io.Bytes;

using haxe.iterators.StringIteratorUnicode;

private enum Filename {
	Only(ref:Array<Int>);
	Normal(nfc:Array<Int>, nfd:Array<Int>);
}

class TestUnicode extends utest.Test {
	static var BIN_PATH =
#if cpp
		"bin/cpp";
#elseif cs
		"bin/cs/bin";
#elseif hl
		"bin/hl";
#elseif java
		"bin/java";
#elseif neko
		"bin/neko";
#elseif php
		"bin/php";
#elseif python
		"bin/python";
#else
		null;
#end
	static var BIN_NAME =
#if cpp
		#if debug
			"UtilityProcess-debug";
		#else
			"UtilityProcess";
		#end
#elseif cs
		#if debug
			"UtilityProcess-Debug.exe";
		#else
			"UtilityProcess.exe";
		#end
#elseif hl
		"UtilityProcess.hl";
#elseif java
		#if debug
			"UtilityProcess-Debug.jar";
		#else
			"UtilityProcess.jar";
		#end
#elseif neko
		"UtilityProcess.n";
#elseif php
		"UtilityProcess/index.php";
#elseif python
		"UtilityProcess.py";
#else
		null;
#end
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

	// list of filenames expected to NOT exist
	static var nonExistentNames:Array<Filename> = [
		// Java escapes
		Only([0x0025, 0x0030 , 0x0001]), // %01
		Only([0x0025, 0x0037 , 0x0046]) // %7F
	];

	// list of expected filenames in Unicode codepoint sequences
	static var names:Array<Filename> = [
		// boundary conditions
		Only([0x0001]),
		Only([0x007F]),
		Only([0x0080]),
		Only([0x07FF]),
		Only([0x0800]),
		Only([0xD7FF]),
		Only([0xE000]),
		Only([0xFFFD]),
		//Only([0xFFFE]), // invalid character!
		//Only([0xFFFF]), // invalid character!
		// non-BMP (not supported for the time being)
		//Only([0x10000]),
		//Only([0x1FFFF]),
		//Only([0xFFFFF]),
		//Only([0x100000]),
		//Only([0x10FFFF]),
		Only([0x1F602, 0x1F604, 0x1F619])
	].concat([
		// NFC / NFD
		Normal([0x0227], [0x0061, 0x0307])
	]);

	static var namesRoot = names.concat([
		// extra files in the root test-res directory
		Only([0x0061]), // a
		Only([0x0062]), // b
		Only([0x64, 0x61, 0x74, 0x61, 0x2E, 0x62, 0x69, 0x6E]) // data.bin
	]);

	function codepointsSame(actual:Array<Int>, expected:Filename):Bool {
		function sameOption(ref:Array<Int>):Bool {
			if (actual.length != ref.length) return false;
			for (i in 0...actual.length) if (actual[i] != ref[i]) return false;
			return true;
		}
		return (switch (expected) {
				case Only(ref): sameOption(ref);
				case Normal(nfc, nfd):
				// it might eventually be best to expect a particular form
				// on specific targets + platforms + filesystems
				// for now, allowing either
				sameOption(nfc) || sameOption(nfd);
			});
	}

	// same names and length, but possibly different order
	// assumes no duplicates in expected
	function sameFiles(actual:Array<String>, expected:Array<Filename>):Void {
		Assert.equals(actual.length, expected.length);
		var remaining = expected.copy();
		for (file in actual) {
			var codepoints = unicodeCodepoints(file);
			var found = null;
			for (ref in remaining) {
				if (codepointsSame(codepoints, ref)) {
					found = ref;
					break;
				}
			}
			if (found == null) {
				Assert.fail('unexpected filename $file (${codepoints.map(c -> StringTools.hex(c, 4))}) found');
			} else {
				remaining.remove(found);
			}
		}
	}

	function unicodeCodepoints(str:String):Array<Int> {
		return [ for (codepoint in str.unicodeIterator()) codepoint ];
	}

	function codepointsToString(ref:Array<Int>):String {
		return [ for (codepoint in ref) String.fromCharCode(codepoint) ].join("");
	}

	function showUString(str:String):String {
		return '$str (${unicodeCodepoints(str)})';
	}

	function assertUEnds(actual:String, expected:String, ?alt:String, ?pos:haxe.PosInfos):Void {
		Assert.isTrue(
			StringTools.endsWith(actual, expected) || (alt != null ? StringTools.endsWith(actual, alt) : false),
			'expected ${showUString(actual)} to end with ${showUString(expected)}'
			+ (alt != null ? ' or ${showUString(alt)}' : ""),
			pos
		);
	}

	function assertUEquals(actual:String, expected:String, ?msg:String, ?pos:haxe.PosInfos):Void {
		Assert.equals(
			expected, actual,
			'expected ${showUString(actual)} to be ${showUString(expected)}',
			pos
		);
	}

	function normalNFC(f:String->Void):Void {
		for (filename in names) switch (filename) {
			case Only(codepointsToString(_) => ref): f(ref);
			case Normal(codepointsToString(_) => nfc, _): f(nfc);
		}
	}

	function normalBoth(f:String->Void):Void {
		for (filename in names) switch (filename) {
			case Only(codepointsToString(_) => ref): f(ref);
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			f(nfc);
			f(nfd);
		}
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

	function runUtility(args:Array<String>, ?options:{?stdin:String, ?execPath:String, ?execName:String}):{
		exitCode:Int,
		stdout:String,
		stderr:String
	} {
		if (options == null) options = {};
		if (options.execPath == null) options.execPath = BIN_PATH;
		if (options.execName == null) options.execName = BIN_NAME;
		var execFull = '${options.execPath}/${options.execName}';
		var proc =
		#if (macro || interp)
		new sys.io.Process("haxe", ["compile-each.hxml", "--run", execFull].concat(args));
		#elseif cpp
		new sys.io.Process(execFull, args);
		#elseif cs
		(switch (Sys.systemName()) {
			case "Windows":
				new sys.io.Process(execFull, args);
			case _:
				new sys.io.Process("mono", [execFull].concat(args));
		});
		#elseif java
		new sys.io.Process(haxe.io.Path.join([java.lang.System.getProperty("java.home"), "bin", "java"]), ["-jar", execFull].concat(args));
		#elseif python
		new sys.io.Process(python.lib.Sys.executable, [execFull].concat(args));
		#elseif neko
		new sys.io.Process("neko", [execFull].concat(args));
		#elseif hl
		new sys.io.Process("hl", [execFull].concat(args));
		#elseif php
		new sys.io.Process(php.Global.defined('PHP_BINARY') ? php.Const.PHP_BINARY : 'php', [execFull].concat(args));
		#elseif lua
		new sys.io.Process("lua", [execFull].concat(args));
		#else
		null;
		#end
		if (options.stdin != null) {
			proc.stdin.writeString(options.stdin);
			proc.stdin.flush();
		}
		return {
			exitCode: proc.exitCode(),
			stdout: proc.stdout.readAll().toString(),
			stderr: proc.stderr.readAll().toString()
		};
	}

#if (target.unicode)
	function testPaths() {
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
				for (symlink in [
						{name: "bin-cpp", target: "/bin/cpp/UtilityProcess-debug"},
						{name: "bin-cs", target: "/bin/cs/bin/UtilityProcess-Debug.exe"},
						{name: "bin-hl", target: "/bin/hl/UtilityProcess.hl"},
						{name: "bin-java", target: "/bin/java/UtilityProcess-Debug.jar"},
						{name: "bin-neko", target: "/bin/neko/UtilityProcess.n"},
						{name: "bin-php", target: "/bin/php/UtilityProcess"},
						{name: "bin-py", target: "/bin/python/UtilityProcess.py"}
					]) assertUEnds(
						sys.FileSystem.fullPath('$path/${symlink.name}'),
						symlink.target
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
	}

	function testIO() {
		// stdin.readLine
		normalBoth(str -> {
				assertUEquals(runUtility(["stdin.readLine"], {stdin: '$str\n'}).stdout, '$str\n');
			});

		// stdin.readString
		normalBoth(str -> {
				// FIXME: readString of UTF8 should maybe read n characters, not bytes?
				var byteLength = Bytes.ofString(str).length;
				assertUEquals(runUtility(["stdin.readString", '${byteLength}'], {stdin: '$str'}).stdout, '$str\n');
			});

		// stdin.readUntil
		normalBoth(str -> {
				// make sure the 0x70 byte is not part of the test string 
				assertUEquals(runUtility(["stdin.readUntil", "0x70"], {stdin: str + "\x70" + str + "\x70"}).stdout, '$str\n');
			});

		// stdout
		normalBoth(str -> {
				assertUEquals(runUtility(["stdout.writeString", str]).stdout, '$str');
			});

		// stderr
		normalBoth(str -> {
				assertUEquals(runUtility(["stderr.writeString", str]).stderr, '$str');
			});

		// readLine
		var data = sys.io.File.read("test-res/data.bin");
		normalNFC(str -> {
				var line = data.readLine();
				assertUEquals(line, str);
			});
	}
#end
}
