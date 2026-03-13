package formatter.codedata;

typedef TokenInfo = {
	var token:TokenTree;
	var whitespaceAfter:WhitespaceAfterType;
	var spacesAfter:Int;
	var spacesBefore:Int;
	var emptyLinesAfter:Int;
	var wrapAfter:Bool;
	var text:String;
	var additionalIndent:Int;
}
