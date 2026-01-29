package unit;

class TestResource extends Test {
	static var STR = "Héllo World !";

	function testResources() {
		var names = haxe.Resource.listNames().filter(function(name) return name != "serializedValues.txt");
		eq(names.length, 2);
		if (names[0] == "re/s?!%[]))(\"'1.txt") {
			// redundant, but let's avoid different test numbers
			eq(names[0], "re/s?!%[]))(\"'1.txt");
			eq(names[1], "re/s?!%[]))(\"'1.bin");
		} else {
			eq(names[0], "re/s?!%[]))(\"'1.bin");
			eq(names[1], "re/s?!%[]))(\"'1.txt");
		}
		eq(haxe.Resource.getString("re/s?!%[]))(\"'1.txt"), STR);
		#if (neko || php || eval)
		// allow binary strings
		eq(haxe.Resource.getBytes("re/s?!%[]))(\"'1.bin").sub(0, 9).toHex(), "48656c6c6f2c204927");
		#else
		// cut until first \0
		eq(haxe.Resource.getString("re/s?!%[]))(\"'1.bin").substr(0, 2), "He");
		#end
		eq(haxe.Resource.getBytes("re/s?!%[]))(\"'1.txt").compare(haxe.io.Bytes.ofString(STR)), 0);
		var b = haxe.Resource.getBytes("re/s?!%[]))(\"'1.bin");
		var firsts = [
			0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x49, 0x27, 0x6D, 0x20, 0x61, 0x20, 0x62, 0x69, 0x6E, 0x61
		];
		var lasts = [
			0x65, 0x20, 0x79, 0x6F, 0x75, 0x20, 0x6E, 0x65, 0x78, 0x74, 0x20, 0x74, 0x69, 0x6D, 0x65, 0x21, 0x00, 0x00
		];
		for (i in 0...firsts.length)
			eq(b.get(i), firsts[i]);
		for (i in 0...lasts.length)
			eq(b.get(b.length - lasts.length + i), lasts[i]);

		eq(haxe.Resource.getString("nope"), null);
		eq(haxe.Resource.getBytes("nope"), null);
	}

	#if neko
	static function main() {
		var ch = sys.io.File.write("re/s?!%[]))(\"'1.txt", true);
		ch.writeString(STR);
		ch.close();
		var ch = sys.io.File.write("re/s?!%[]))(\"'1.bin", true);
		ch.writeString("Héllo");
		ch.writeByte(0);
		ch.writeString("World");
		ch.writeInt32(0);
		ch.writeString("!");
		ch.close();
	}
	#end
}
