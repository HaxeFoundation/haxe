package formatter.codedata;

import formatter.config.Config;
import tokentree.TokenTreeBuilder.TokenTreeEntryPoint;

typedef FormatterInputData = {
	var fileName:String;
	var content:Bytes;
	@:optional var tokenList:Array<Token>;
	@:optional var tokenTree:TokenTree;
	var config:Config;
	@:optional var entryPoint:TokenTreeEntryPoint;
	@:optional var lineSeparator:String;
	@:optional var range:FormatterInputRange;
	@:optional var indentOffset:Int;
}

typedef FormatterInputRange = {
	var startPos:Int;
	var endPos:Int;
}
