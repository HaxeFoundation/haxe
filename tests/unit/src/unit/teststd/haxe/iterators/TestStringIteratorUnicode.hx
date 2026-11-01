package unit.teststd.haxe.iterators;

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

		aeq(["a".code, "b".code, "c".code, "d".code, "e".code], traverse("abcde"));
		aeq(["a".code, "a".code, "😂".code, "é".code, "é".code], traverse("aa😂éé"));

		var surrogateBorders = [
			"𐀀", //D800,DC00 - U+10000
			"𐏿", //D800,DFFF - U+103FF
			"􏰀", //DBFF,DC00 - U+10FC00
			"􏿿", //DBFF,DFFF - U+10FFFF
		];
		var rStr = traverse(surrogateBorders.join(''));
		aeq([65536, 66559, 1113088, 1114111], rStr);

		#else
		eq(1, 1);
		#end
	}
}
