function traverse(s:String) {
	var ak = [];
	var av = [];
	for (offset => code in new haxe.iterators.StringKeyValueIteratorUnicode(s)) {
		ak.push(offset);
		av.push(code);
	}
	return { k: ak, v: av };
}

#if !(neko || (cpp && !cppia && !hxcpp_smart_strings))

var r = traverse("abcde");
r.k == [0, 1, 2, 3, 4];
r.v == ["a".code, "b".code, "c".code, "d".code, "e".code];

var r = traverse("aa😂éé");
r.k == [0, 1, 2, 3, 4];
r.v == ["a".code, "a".code, "😂".code, "é".code, "é".code];

#else
1 == 1;

#end