package flash.text.engine;

extern class ElementFormat {
	function new(?fontDescription : flash.text.engine.FontDescription, ?fontSize : Float, ?color : UInt, ?alpha : Float, ?textRotation : flash.text.engine.TextRotation, ?dominantBaseline : flash.text.engine.TextBaseline, ?alignmentBaseline : flash.text.engine.TextBaseline, ?baselineShift : Float, ?kerning : flash.text.engine.Kerning, ?trackingRight : Float, ?trackingLeft : Float, ?locale : String, ?breakOpportunity : flash.text.engine.BreakOpportunity, ?digitCase : flash.text.engine.DigitCase, ?digitWidth : flash.text.engine.DigitWidth, ?ligatureLevel : flash.text.engine.LigatureLevel, ?typographicCase : flash.text.engine.TypographicCase) : Void;
	var alignmentBaseline : flash.text.engine.TextBaseline;
	var alpha : Float;
	var baselineShift : Float;
	var breakOpportunity : flash.text.engine.BreakOpportunity;
	function clone() : flash.text.engine.ElementFormat;
	var color : UInt;
	var digitCase : flash.text.engine.DigitCase;
	var digitWidth : flash.text.engine.DigitWidth;
	var dominantBaseline : flash.text.engine.TextBaseline;
	var fontDescription : flash.text.engine.FontDescription;
	var fontSize : Float;
	function getFontMetrics() : flash.text.engine.FontMetrics;
	var kerning : flash.text.engine.Kerning;
	var ligatureLevel : flash.text.engine.LigatureLevel;
	var locale : String;
	var locked : Bool;
	var textRotation : flash.text.engine.TextRotation;
	var trackingLeft : Float;
	var trackingRight : Float;
	var typographicCase : flash.text.engine.TypographicCase;
}
