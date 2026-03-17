package unit.std.haxe.iterators;

class TestStringIteratorUnicode extends unit.Test {
	public function test() {
		function traverse(s:String) {
			var a = [];
			for (code in new haxe.iterators.StringIteratorUnicode(s)) {
				a.push(code);
			}
			return a;
		}

		#if (target.unicode || neko)

		eq(traverse("abcde")[0], "a".code);
		eq(traverse("abcde")[1], "b".code);
		eq(traverse("abcde")[2], "c".code);
		eq(traverse("abcde")[3], "d".code);
		eq(traverse("abcde")[4], "e".code);
		eq(traverse("aa😂éé")[0], "a".code);
		eq(traverse("aa😂éé")[1], "a".code);
		eq(traverse("aa😂éé")[2], "😂".code);
		eq(traverse("aa😂éé")[3], "é".code);
		eq(traverse("aa😂éé")[4], "é".code);

		var surrogateBorders = [
			"𐀀", //D800,DC00 - U+10000
			"𐏿", //D800,DFFF - U+103FF
			"􏰀", //DBFF,DC00 - U+10FC00
			"􏿿", //DBFF,DFFF - U+10FFFF
		];
		var rStr = traverse(surrogateBorders.join(''));
		rStr == [
			65536,	//D800,DC00 - U+10000
			66559,	//D800,DFFF - U+103FF
			1113088,//DBFF,DC00 - U+10FC00
			1114111	//DBFF,DFFF - U+10FFFF
		];

		#else
		eq(1, 1);
		#end
	}
}
