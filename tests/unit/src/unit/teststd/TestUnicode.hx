package unit.teststd;

class TestUnicode extends unit.Test {
	public function test() {
		#if target.unicode // neko and cpp with -D disable_unicode_strings will not be made Unicode compatible


		var s = String.fromCharCode(0xE9);
		eq(s, "é");
		eq(s.length, 1);
		eq(s.charCodeAt(0), 0xE9);

		var s = String.fromCharCode("あ".code);
		eq(s, "あ");
		eq(s.length, 1);
		eq(s.charCodeAt(0), "あ".code);

		//outside of BMP
		var s = String.fromCharCode("𠜎".code);
		eq(s, "𠜎");

		var s = "aa😂éé";
		t(s.indexOf(String.fromCharCode(0x80)) < 0);
		eq(s.indexOf("é"), s.length-2);
		eq(s.indexOf("aa"), 0);
		eq(s.indexOf("a"), 0);
		eq(s.lastIndexOf("a"), 1);
		t(s.indexOf("😂") > 0);
		t(s.lastIndexOf("😂") > 0);
		eq(s.lastIndexOf("é"), s.length-1);
		var s = "abc";
		t(s.indexOf("éé") < 0);
		t(s.lastIndexOf("éé") < 0);

		eq("012::345€".indexOf("::", 1), 3);

		var s = String.fromCharCode(0x1f602);
		eq(s, "😂");


		#if !utf16
		// native UTF-16 or 32
		eq(s.length, 1);
		eq(s.charCodeAt(0), "😂".code);
		#else
		// UTF-16 surrogate pairs encoding
		eq(s.length, 2);
		eq(s.charCodeAt(0), 0xD83D);
		eq(s.charCodeAt(1), 0xDE02);
		#end

		eq("\u00E9\u3042", "éあ");
		// "\uD83D\uDE02" == "😂" // gives Invalid Unicode char, that's correct
		// maybe later we can add support for \U******** for out of BMP escape sequence

		var s = "é" + "あ";
		eq(s, "éあ");
		eq(s.length, 2);
		eq(s.charCodeAt(1), "あ".code);

		var s = "é" + "😂" + "あ";
		eq(s, "é😂あ");
		var a = s.split('😂');
		eq(a.length, 2);
		aeq(["é", "あ"], a);
		eq(a.join('😂'), s);

		var a = s.split('');
		#if !utf16
		// native UTF-16 or 32
		eq(a.length, 3);
		aeq(["é", "😂", "あ"], a);
		#else
		eq(a.length, 4);
		eq(a[0], "é");
		eq(a[3], "あ");
		#end

		var buf = new StringBuf();
		buf.addChar(0xE9);
		buf.addChar("あ".code);
		buf.add("é");
		buf.add("あ");
		var str = buf.toString();
		eq(str.length, 4);
		eq(str, "éあéあ");
		eq(str.charCodeAt(3), "あ".code);

		var str = StringTools.urlEncode("éあ😂");
		eq(str, "%C3%A9%E3%81%82%F0%9F%98%82");
		str = StringTools.urlDecode(str);
		eq(str, "éあ😂");

		var str = haxe.Serializer.run("éあ");
		eq(str, "y15:%C3%A9%E3%81%82");
		str = haxe.Unserializer.run(str);
		eq(str, "éあ");

		var str = haxe.Serializer.run("😂");
		eq(str, "y12:%F0%9F%98%82");
		str = haxe.Unserializer.run(str);
		eq(str, "😂");

		var str = haxe.io.Bytes.ofString("éあ😂");
		eq(str.toHex(), "c3a9e38182f09f9882");

		eq(["é", "e"].join("é"), "éée");
		eq(["é", "e"].join("e"), "éee");

		var rawBytes = haxe.io.Bytes.ofString("éあ😂",RawNative);

		#if (!utf16 || cpp)
		rawBytes.toHex() == "c3a9e38182f09f9882"; // UTF-8 native
		#else
		rawBytes.toHex() == "e90042303dd802de"; // UTF-16 native
		#end

		eq(rawBytes.getString(0,rawBytes.length,RawNative), "éあ😂");

		eq(haxe.crypto.Md5.encode("éあ😂"), "d30b209e81e40d03dd474b26b77a8a18");
		eq(haxe.crypto.Sha1.encode("éあ😂"), "ec79856a75c98572210430aeb7fe6300b6c4e20c");
		//haxe.crypto.Sha224.encode("éあ😂") == "d7967c5f27bd6868e276647583c55ab09d5f45b40610a3d9c6d91b90";
		//haxe.crypto.Sha256.encode("éあ😂") == "d0230b8d8ac2d6d0dbcee11ad0e0eaa68a6565347261871dc241571cab591676";
		eq(haxe.crypto.BaseCode.encode("éあ😂","0123456789abcdef"), "c3a9e38182f09f9882");

		var buf = new haxe.io.BytesBuffer();
		buf.addString("éあ😂");
		buf.addString("éあ😂",RawNative);
		var bytes = buf.getBytes();
		eq(bytes.getString(0,9), "éあ😂");
		eq(bytes.getString(2,3), "あ");
		eq(bytes.getString(5,4), "😂");
		eq(bytes.getString(2,7), "あ😂");
		eq(bytes.getString(9,bytes.length - 9,RawNative), "éあ😂");
		eq(bytes.sub(9,bytes.length - 9).compare(rawBytes), 0);

		var o = new haxe.io.BytesOutput();
		o.writeString("éあ😂");
		o.writeString("éあ😂",RawNative);
		var bytes2 = o.getBytes();
		eq(bytes2.toHex(), bytes.toHex());

		var input = new haxe.io.BytesInput(bytes2);
		eq(input.readString(2), "é");
		eq(input.readString(7), "あ😂");
		eq(input.readString(bytes.length - 9,RawNative), "éあ😂");

		var s = "ée";
		var s1 = s.charAt(1);
		eq(s1, "e");

		var s1 = s.substr(1, 1);
		var s2 = s.substr(1);
		var s3 = s.substr(-1);
		var s4 = s.substr(-1, 1);
		eq(s1, "e");
		eq(s2, "e");
		eq(s3, "e");
		eq(s4, "e");

		var s1 = s.substring(1, 2);
		var s2 = s.substring(1);
		var s3 = s.substring(2, 1);
		var s4 = s.substring(1, 20);
		eq(s1, "e");
		eq(s2, "e");
		eq(s3, "e");
		eq(s4, "e");

		t(Reflect.compare("ed", "éee".substr(1)) < 0);
		t(Reflect.compare("éed".substr(1), "éee".substr(1)) < 0);
		t(Reflect.compare("éed".substr(1), "ee") < 0);
		t(Reflect.compare("ee", "éed".substr(1)) > 0);
		t(Reflect.compare("éee".substr(1), "éed".substr(1)) > 0);
		t(Reflect.compare("éee".substr(1), "ed") > 0);

		var s = "ä😂";
		eq(s.toUpperCase(), "Ä😂");
		eq(s.toLowerCase(), s);

		var s = "Ä😂";
		eq(s.toUpperCase(), s);
		eq(s.toLowerCase(), "ä😂");

		var s = "a😂";
		eq(s.toUpperCase(), "A😂");
		eq(s.toLowerCase(), s);

		var s = "A😂";
		eq(s.toUpperCase(), s);
		eq(s.toLowerCase(), "a😂");

		eq("σ".toUpperCase(), "Σ");
		eq("Σ".toLowerCase(), "σ");

		var map = new haxe.ds.StringMap();
		map.set("path", 1);
		eq(map.get("äpath".substr(1)), 1);

		var data =  "<haxe><s>Hello World!</s><s2>π</s2></haxe>";
		var buf = new StringBuf();
		buf.addSub(data, 9, 12);
		var s = buf.toString();
		eq(s, "Hello World!");
		eq(s.length, 12);

		aeq(["abc", "def"], "äabc:def".substr(1).split(":"));

		var s1 = "abc";
		var b1 = haxe.io.Bytes.ofString(s1, RawNative);
		var s2 = b1.getString(0, b1.length, RawNative);
		eq(s1, s2);

		var obj:Dynamic = { abc: "ok" };
		var field = "äabc".substr(1);
		eq(Reflect.field(obj, field), "ok");
		t(Reflect.hasField(obj, field));
		t(Reflect.deleteField(obj, field));
		f(Reflect.deleteField(obj, field));
		f(Reflect.hasField(obj, field));
		eq(Reflect.field(obj, field), null);

		var obj:Dynamic = { };
		Reflect.setField(obj, field, "still ok");
		eq(Reflect.field(obj, field), "still ok");
		t(Reflect.hasField(obj, field));
		t(Reflect.deleteField(obj, field));
		f(Reflect.deleteField(obj, field));
		f(Reflect.hasField(obj, field));
		eq(Reflect.field(obj, field), null);

		// EReg -_-

		function test(left:String, middle:String, right:String, ?rex:EReg) {
			var s = '$left:$middle:$right';
			if (rex == null) {
				rex = new EReg(':($middle):', "");
			}
			function check(rex:EReg) {
				eq(rex.matchedLeft(), left);
				eq(rex.matchedRight(), right);
				eq(rex.matched(1), middle);
				var pos = rex.matchedPos();
				eq(pos.pos, left.length);
				eq(pos.len, middle.length + 2);
			}

			t(rex.match(s));
			check(rex);

			var split = rex.split(s);
			eq(2, split.length);
			eq(left, split[0]);
			eq(right, split[1]);

			eq(rex.replace(s, "a"), '${left}a$right');
			eq(rex.replace(s, "ä"), '${left}ä$right');

			eq(rex.map(s, r -> {
				check(r);
				"a";
			}), '${left}a$right');

			eq(rex.map(s, r -> {
				check(r);
				"ä";
			}), '${left}ä$right');
		}

		test("äb", "ä", "bc");
		test("äb", "a", "bc");
		test("ab", "a", "bc");
		test("ab", "ä", "bc");

		test("äb", "äbc", "bc");
		test("äb", "abc", "bc");
		test("ab", "abc", "bc");
		test("ab", "äbc", "bc");

		test("あb", "あbc", "bc");
		test("あb", "abc", "bc");
		test("ab", "abc", "bc");
		test("ab", "あbc", "bc");

		#if !flash
		// wontfix (cantfix?)
		test("😂b", "😂bc", "bc");
		test("😂b", "abc", "bc");
		test("ab", "abc", "bc");
		test("ab", "😂bc", "bc");
		#end

		#if (eval || lua || python)
		// unspecced?
		test("()", "ä", "[]", ~/:(\w):/);
		f(~/\bx/.match("äx"));
		f(~/x\b/.match("xä"));
		#end

		test("a", "É", "b", ~/:(é):/i);
		test("a", "é", "b", ~/:(É):/i);

		#else
		eq(1, 1);
		#end

		//Border values for surrogate pairs
		"𐀀".code == 65536; //D800,DC00 - U+10000
		"𐏿".code == 66559; //D800,DFFF - U+103FF
		"􏰀".code == 1113088; //DBFF,DC00 - U+10FC00
		"􏿿".code == 1114111; //DBFF,DFFF - U+10FFFF

	}
}
