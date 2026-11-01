package unit.teststd.haxe.iterators;

class TestStringKeyValueIteratorUnicode extends unit.Test {
	public function test() {
		function traverse(s:String) {
			var ak = [];
			var av = [];
			for (offset => code in new haxe.iterators.StringKeyValueIteratorUnicode(s)) {
				ak.push(offset);
				av.push(code);
			}
			return { k: ak, v: av };
		}

		#if (target.unicode || neko)

		var r = traverse("abcde");
		aeq([0, 1, 2, 3, 4], r.k);
		aeq(["a".code, "b".code, "c".code, "d".code, "e".code], r.v);

		var r = traverse("aa😂éé");
		aeq([0, 1, 2, 3, 4], r.k);
		aeq(["a".code, "a".code, "😂".code, "é".code, "é".code], r.v);

		var surrogateBorders = [
			"𐀀", //D800,DC00 - U+10000
			"𐏿", //D800,DFFF - U+103FF
			"􏰀", //DBFF,DC00 - U+10FC00
			"􏿿", //DBFF,DFFF - U+10FFFF
		];
		var rStr = traverse(surrogateBorders.join(''));
		aeq([0, 1, 2, 3], rStr.k);
		aeq([65536, 66559, 1113088, 1114111], rStr.v);

		#else
		eq(1, 1);

		#end
	}
}
