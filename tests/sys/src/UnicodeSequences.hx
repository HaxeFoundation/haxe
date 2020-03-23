using haxe.iterators.StringIteratorUnicode;

enum UnicodeString {
	Only(ref:Array<Int>);
	Normal(nfc:Array<Int>, nfd:Array<Int>);
}

class UnicodeSequences {
	// boundary conditions
	public static var boundary:Array<UnicodeString> = [
		// 1 byte
		Only([0x0001]), // this must be first, see TestUnicode.names
		Only([0x007F]),
		// 2 byte
		Only([0x0080]),
		Only([0x07FF]),
		// 3 byte
		Only([0x0800]),
		Only([0xD7FF]), // just before surrogates
		Only([0xE000]), // just after surrogates
		Only([0xFFFD]),
		// non-BMP (4 byte)
		Only([0x10000]),
		Only([0x1FFFF]),
		Only([0xFFFFF]),
		Only([0x100000]),
		Only([0x10FFFF])
	];

	// NFC / NFD
	public static var normal:Array<UnicodeString> = [
		Normal([0x0227], [0x0061, 0x0307]),
		Normal([0x4E2D, 0x6587, 0xFF0C, 0x306B, 0x307B, 0x3093, 0x3054], [0x4E2D, 0x6587, 0xFF0C, 0x306B, 0x307B, 0x3093, 0x3053, 0x3099])
	];

	// valid sequences
	public static var valid:Array<UnicodeString> =
		boundary
		.concat([Only([0x1F602, 0x1F604, 0x1F619])]) // important (non-BMP) emoji
		.concat(normal);

	public static var validFilenames:Array<UnicodeString> = {
		var valid = valid.copy();
		if (Sys.systemName() == "Windows") valid = valid.filter(f -> !f.match(Only([0x0001])));
		#if !(TEST_INVALID_UNICODE_FS)
		valid = valid.filter(f ->
			!f.match(Only([0xD7FF]))
			&& !f.match(Only([0x1FFFF]))
			&& !f.match(Only([0xFFFFF]))
			&& !f.match(Only([0x10FFFF])));
		#end
		valid;
	};

	public static var validBytes = haxe.io.Bytes.ofHex(
			"010A" +
			"7F0A" +
			"C2800A" +
			"DFBF0A" +
			"E0A0800A" +
			"ED9FBF0A" +
			"EE80800A" +
			"EFBFBD0A" +
			"F09080800A" +
			"F09FBFBF0A" +
			"F3BFBFBF0A" +
			"F48080800A" +
			"F48FBFBF0A" +
			"F09F9882F09F9884F09F98990A" +
			"C8A70A" +
			"E4B8ADE69687EFBC8CE381ABE381BBE38293E381940A"
		);

	public static var validString =
		"\u0001\n" +
		"\u007F\n" +
		"\u0080\n" +
		"\u07FF\n" +
		"\u0800\n" +
		"\uD7FF\n" +
		"\uE000\n" +
		"\uFFFD\n" +
		"\u{10000}\n" +
		"\u{1FFFF}\n" +
		"\u{FFFFF}\n" +
		"\u{100000}\n" +
		"\u{10FFFF}\n" +
		"\u{1F602}\u{1F604}\u{1F619}\n" +
		"\u0227\n" +
		"\u4E2D\u6587\uFF0C\u306B\u307B\u3093\u3054\n";

	// utility methods

	public static function unicodeCodepoints(str:String):Array<Int> {
		return [ for (codepoint in str.unicodeIterator()) codepoint ];
	}

	public static function codepointsToString(ref:Array<Int>):String {
		return [ for (codepoint in ref) String.fromCharCode(codepoint) ].join("");
	}

	public static function showUnicodeString(str:String):String {
		return '$str (${unicodeCodepoints(str)})';
	}

	public static function codepointsSame(actual:Array<Int>, expected:UnicodeString):Bool {
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

	public static function normalNFC(f:String->Void):Void {
		for (seq in valid) switch (seq) {
			case Only(codepointsToString(_) => ref): f(ref);
			case Normal(codepointsToString(_) => nfc, _): f(nfc);
		}
	}

	public static function normalBoth(f:String->Void):Void {
		for (seq in valid) switch (seq) {
			case Only(codepointsToString(_) => ref): f(ref);
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			f(nfc);
			f(nfd);
		}
	}

	public static function normalBothIndexed(f:String->Int->Bool->Void):Void {
		for (i in 0...valid.length) switch (valid[i]) {
			case Only(codepointsToString(_) => ref): f(ref, i, false);
			case Normal(codepointsToString(_) => nfc, codepointsToString(_) => nfd):
			f(nfc, i, true);
			f(nfd, i, false);
		}
	}
}
