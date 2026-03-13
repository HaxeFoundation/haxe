package formatter.marker.wrapping;

typedef WrappableItem = {
	var first:TokenTree;
	var last:TokenTree;
	var multiline:Bool;
	var firstLineLength:Int;
	var lastLineLength:Int;
}
