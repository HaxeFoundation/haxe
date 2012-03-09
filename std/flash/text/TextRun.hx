package flash.text;

extern class TextRun {
	var beginIndex : Int;
	var endIndex : Int;
	var textFormat : TextFormat;
	function new(beginIndex : Int, endIndex : Int, textFormat : TextFormat) : Void;
}
