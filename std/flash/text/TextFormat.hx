package flash.text;

extern class TextFormat {
	var align : TextFormatAlign;
	var blockIndent : Null<Float>;
	var bold : Null<Bool>;
	var bullet : Null<Bool>;
	var color : Null<UInt>;
	var display : TextFormatDisplay;
	var font : String;
	var indent : Null<Float>;
	var italic : Null<Bool>;
	var kerning : Null<Bool>;
	var leading : Null<Float>;
	var leftMargin : Null<Float>;
	var letterSpacing : Null<Float>;
	var rightMargin : Null<Float>;
	var size : Null<Float>;
	var tabStops : Array<UInt>;
	var target : String;
	var underline : Null<Bool>;
	var url : String;
	function new(?font : String, size : Null<Float> = 0, color : Null<UInt> = 0, bold : Null<Bool> = false, italic : Null<Bool> = false, underline : Null<Bool> = false, ?url : String, ?target : String, ?align : TextFormatAlign, leftMargin : Null<Float> = 0, rightMargin : Null<Float> = 0, indent : Null<Float> = 0, leading : Null<Float> = 0) : Void;
}
