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
r.k == [0, 1, 2, 3, 4];
r.v == ["a".code, "b".code, "c".code, "d".code, "e".code];

var r = traverse("aa😂éé");
r.k == [0, 1, 2, 3, 4];
r.v == ["a".code, "a".code, "😂".code, "é".code, "é".code];

var surrogateBorders = [
	"𐀀", //D800,DC00 - U+10000
	"𐏿", //D800,DFFF - U+103FF
	"􏰀", //DBFF,DC00 - U+10FC00
	"􏿿", //DBFF,DFFF - U+10FFFF
];
var rStr = traverse(surrogateBorders.join(''));
rStr.k == [0, 1, 2, 3];
rStr.v == [
	65536,	//D800,DC00 - U+10000
	66559,	//D800,DFFF - U+103FF
	1113088,//DBFF,DC00 - U+10FC00
	1114111	//DBFF,DFFF - U+10FFFF
];

#else
1 == 1;

#end