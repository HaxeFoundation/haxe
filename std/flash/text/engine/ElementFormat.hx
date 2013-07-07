package flash.text.engine;

@:final extern class ElementFormat {
	var alignmentBaseline : TextBaseline;
	var alpha : Float;
	var baselineShift : Float;
	var breakOpportunity : BreakOpportunity;
	var color : UInt;
	var digitCase : DigitCase;
	var digitWidth : DigitWidth;
	var dominantBaseline : TextBaseline;
	var fontDescription : FontDescription;
	var fontSize : Float;
	var kerning : Kerning;
	var ligatureLevel : LigatureLevel;
	var locale : String;
	var locked : Bool;
	var textRotation : TextRotation;
	var trackingLeft : Float;
	var trackingRight : Float;
	var typographicCase : TypographicCase;
	function new(?fontDescription : FontDescription, fontSize : Float = 12, color : UInt = 0, alpha : Float = 1, ?textRotation : TextRotation, ?dominantBaseline : TextBaseline, ?alignmentBaseline : TextBaseline, baselineShift : Float = 0, ?kerning : Kerning, trackingRight : Float = 0, trackingLeft : Float = 0, ?locale : String, ?breakOpportunity : BreakOpportunity, ?digitCase : DigitCase, ?digitWidth : DigitWidth, ?ligatureLevel : LigatureLevel, ?typographicCase : TypographicCase) : Void;
	function clone() : ElementFormat;
	function getFontMetrics() : FontMetrics;
}
