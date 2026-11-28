class Main {
	static function check(name:String, actual:String, expected:String) {
		if (actual != expected) {
			Sys.println('$name: expected $expected but got $actual');
		}
	}

	static function main() {
		// Without lua-utf8, code points must still be encoded as UTF-8
		check("1 byte", String.fromCharCode(0x41), "A");
		check("2 bytes", String.fromCharCode(0xE9), "é");
		check("3 bytes", String.fromCharCode(0x3042), "あ");
		check("4 bytes", String.fromCharCode(0x1F600), "😀");

		final buf = new StringBuf();
		buf.add("a");
		buf.addChar(0xE9);
		buf.addChar(0x1F600);
		check("StringBuf", buf.toString(), "aé😀");
		check("StringBuf.length", Std.string(buf.length), Std.string("aé😀".length));

		check("Json", haxe.Json.parse('"\\u00e9\\ud83d\\ude00"'), "é😀");
		Sys.println("Done");
	}
}
