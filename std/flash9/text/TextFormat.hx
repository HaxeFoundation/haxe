package flash.text;

extern class TextFormat {
	var align : TextFormatAlign;
	var blockIndent : Dynamic;
	var bold : Dynamic;
	var bullet : Dynamic;
	var color : Dynamic;
	var display : TextFormatDisplay;
	var font : String;
	var indent : Dynamic;
	var italic : Dynamic;
	var kerning : Dynamic;
	var leading : Dynamic;
	var leftMargin : Dynamic;
	var letterSpacing : Dynamic;
	var rightMargin : Dynamic;
	var size : Dynamic;
	var tabStops : Array<UInt>;
	var target : String;
	var underline : Dynamic;
	var url : String;
	function new(?font : String, ?size : Dynamic, ?color : Dynamic, ?bold : Dynamic, ?italic : Dynamic, ?underline : Dynamic, ?url : String, ?target : String, ?align : String, ?leftMargin : Dynamic, ?rightMargin : Dynamic, ?indent : Dynamic, ?leading : Dynamic) : Void;
}
