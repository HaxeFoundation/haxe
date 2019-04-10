import utest.Assert;

using haxe.iterators.StringIteratorUnicode;

private enum Filename {
	Only(ref:Array<Int>);
	Normal(nfc:Array<Int>, nfd:Array<Int>);
}

class TestUnicode extends utest.Test {
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

	function assertEnds(actual:String, expected:String, ?alt:String):Void {
		Assert.isTrue(
			StringTools.endsWith(actual, expected) || (alt != null ? StringTools.endsWith(actual, alt) : false),
			'expected $actual to end with $expected' + (alt != null ? ' or $alt' : "")
		);
	}

#if (target.unicode)
	function testSys() {
#if !(java)
		// setCwd + getCwd
		Sys.setCwd("test-res");
		function enterLeave(dir:String, ?alt:String):Void {
			Sys.setCwd(dir);
			assertEnds(Sys.getCwd(), '/test-res/${dir}', alt != null ? '/test-res/${alt}' : null);
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
	}

	function testFileSystem() {
		function normalBoth(f:String->Void, path:String):Void {
			for (filename in names) switch (filename) {
				case Only(codepointsToString(_) => ref):
				f('$path/$ref');
				case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
				f('$path/$nfc');
				f('$path/$nfd');
			}
		}
		function assertNormalEither(f:String->Bool, path:String, ?msg:String):Void {
			for (filename in names) Assert.isTrue(switch (filename) {
				case Only(codepointsToString(_) => ref): f('$path/$ref');
				case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
				f('$path/$nfc') || f('$path/$nfd');
			}, '$msg ($filename in $path)');
		}

		// absolutePath
		normalBoth(path -> {
				if (!sys.FileSystem.exists(path)) return; // NFC/NFD differences
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
					]) assertEnds(
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
		normalBoth(path -> {
				if (!sys.FileSystem.exists(path)) return; // NFC/NFD differences
				for (symlink in [
						{name: "bin-cpp", target: "/bin/cpp/Main-debug"},
						{name: "bin-cs", target: "/bin/cs/bin/Main-Debug.exe"},
						{name: "bin-hl", target: "/bin/hl/sys.hl"},
						{name: "bin-java", target: "/bin/java/Main-Debug.jar"},
						{name: "bin-neko", target: "/bin/neko/sys.n"},
						{name: "bin-php", target: "/bin/php/Main"},
						{name: "bin-py", target: "/bin/python/sys.py"}
					]) assertEnds(
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
#end
}
