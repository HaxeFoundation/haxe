using haxe.iterators.StringIteratorUnicode;

enum UnicodeString {
	Only(ref:Array<Int>);
	Normal(nfc:Array<Int>, nfd:Array<Int>);
}

class UnicodeSequences {
	// boundary conditions
	public static var boundary:Array<UnicodeString> = [
		Only([0x0001]),
		Only([0x007F]),
		Only([0x0080]),
		Only([0x07FF]),
		Only([0x0800]),
		Only([0xD7FF]),
		Only([0xE000]),
		Only([0xFFFD])
	];
	
	// NFC / NFD
	public static var normal:Array<UnicodeString> = [
		Normal([0x0227], [0x0061, 0x0307])
	];
	
	// non-BMP characters (U+10000 and larger), not fully supported
	public static var nonBMP:Array<UnicodeString> = [
		Only([0x10000]),
		Only([0x1FFFF]),
		Only([0xFFFFF]),
		Only([0x100000]),
		Only([0x10FFFF])
	];
	
	// valid sequences
	public static var valid:Array<UnicodeString> =
		boundary
		// .concat(nonBMP)
		.concat([Only([0x1F602, 0x1F604, 0x1F619])]) // important (non-BMP) emoji
		.concat(normal);
	
	// invalid sequences
	public static var invalid:Array<UnicodeString> = [
		Only([0xFFFE]),
		Only([0xFFFF])
	];

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
}
