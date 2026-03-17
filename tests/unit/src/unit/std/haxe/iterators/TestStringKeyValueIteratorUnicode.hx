package unit.std.haxe.iterators;

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
		eq(r.k[0], 0);
		eq(r.k[1], 1);
		eq(r.k[2], 2);
		eq(r.k[3], 3);
		eq(r.k[4], 4);
		eq(r.v[0], "a".code);
		eq(r.v[1], "b".code);
		eq(r.v[2], "c".code);
		eq(r.v[3], "d".code);
		eq(r.v[4], "e".code);

		var r = traverse("aa😂éé");
		eq(r.k[0], 0);
		eq(r.k[1], 1);
		eq(r.k[2], 2);
		eq(r.k[3], 3);
		eq(r.k[4], 4);
		eq(r.v[0], "a".code);
		eq(r.v[1], "a".code);
		eq(r.v[2], "😂".code);
		eq(r.v[3], "é".code);
		eq(r.v[4], "é".code);

		var surrogateBorders = [
			"𐀀", //D800,DC00 - U+10000
			"𐏿", //D800,DFFF - U+103FF
			"􏰀", //DBFF,DC00 - U+10FC00
			"􏿿", //DBFF,DFFF - U+10FFFF
		];
		var rStr = traverse(surrogateBorders.join(''));
		eq(rStr.k[0], 0);
		eq(rStr.k[1], 1);
		eq(rStr.k[2], 2);
		eq(rStr.k[3], 3);
		rStr.v == [
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
