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
	function new(?font : String, ?size : Float, ?color : UInt, ?bold : Bool, ?italic : Bool, ?underline : Bool, ?url : String, ?target : String, ?align : TextFormatAlign, ?leftMargin : Float, ?rightMargin : Float, ?indent : Float, ?leading : Float) : Void;
}
