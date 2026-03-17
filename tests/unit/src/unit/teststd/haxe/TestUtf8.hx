package unit.teststd.haxe;

class TestUtf8 extends unit.Test {
	@:haxe.warning("-WDeprecated")
	public function test() {
		#if false
		// disabled tests with outside BMP chars (will be reenabled when we support them)
		var str = "あ𠀀い";
		eq(haxe.Utf8.length(str), 3);
		eq(haxe.Utf8.charCodeAt(str, 0), 0x3042);
		eq(haxe.Utf8.charCodeAt(str, 1), 0x20000);
		eq(haxe.Utf8.charCodeAt(str, 2), 0x3044);
		var buf = new haxe.Utf8();
		buf.addChar(0x3042);
		buf.addChar(0x20000);
		buf.addChar(0x3044);
		eq(buf.toString(), str);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 3), str), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 2), "あ𠀀"), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 2), "𠀀い"), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 0), ""), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 0), ""), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 9, 0), ""), 0);
		#end


		// same tests with BMP chars (actually UCS2 compliance only)
		var str = "あéい";
		eq(haxe.Utf8.length(str), 3);
		eq(haxe.Utf8.charCodeAt(str, 0), 0x3042);
		eq(haxe.Utf8.charCodeAt(str, 1), 0xE9);
		eq(haxe.Utf8.charCodeAt(str, 2), 0x3044);
		var big = new haxe.Utf8(10);
		eq(big.toString().length, 0);
		var buf = new haxe.Utf8();
		buf.addChar(0x3042);
		buf.addChar(0xE9);
		buf.addChar(0x3044);
		eq(buf.toString(), str);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 3), str), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 2), "あé"), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 2), "éい"), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 0, 0), ""), 0);
		eq(haxe.Utf8.compare(haxe.Utf8.sub(str, 1, 0), ""), 0);

		// unspecify outside of range Utf8.sub
		// haxe.Utf8.compare(haxe.Utf8.sub(str, 9, 0), "") == 0;

		// #if (neko || php || cpp || lua || macro)
		// TODO neko, cpp, macro
		#if php
		t(haxe.Utf8.validate(haxe.io.Bytes.ofHex("f0a9b8bde38182c3ab61").toString()));
		t(haxe.Utf8.validate(haxe.io.Bytes.ofHex("ed9fbf").toString()));
		t(haxe.Utf8.validate(haxe.io.Bytes.ofHex("ee8080").toString()));
		t(haxe.Utf8.validate(haxe.io.Bytes.ofHex("f48fbfbf").toString()));
		f(haxe.Utf8.validate(haxe.io.Bytes.ofHex("f0a9b8bde381c3ab61").toString()));
		haxe.Utf8.validate(haxe.io.Bytes.ofHex("c0af").toString()) == false; // redundant sequence
		haxe.Utf8.validate(haxe.io.Bytes.ofHex("eda080").toString()) == false; // surrogate byte sequence
		haxe.Utf8.validate(haxe.io.Bytes.ofHex("edbfbf").toString()) == false; // surrogate byte sequence
		haxe.Utf8.validate(haxe.io.Bytes.ofHex("f4908080").toString()) == false; // U+110000
		#end
	}
}
