function traverse(s:String) {
	var a = [];
	for (code in new haxe.iterators.StringIteratorUnicode(s)) {
		a.push(code);
	}
	return a;
}

traverse("abcde") == ["a".code, "b".code, "c".code, "d".code, "e".code];
traverse("aa😂éé") == ["a".code, "a".code, "😂".code, "é".code, "é".code];
