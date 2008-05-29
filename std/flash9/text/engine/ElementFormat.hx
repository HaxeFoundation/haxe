package flash.text.engine;

extern class ElementFormat {
	function new(?fontDescription : flash.text.engine.FontDescription, ?fontSize : Float, ?color : UInt, ?alpha : Float, ?textRotation : String, ?dominantBaseline : String, ?alignmentBaseline : String, ?baselineShift : Float, ?kerning : String, ?tracking : Float, ?locale : String, ?breakOpportunity : String, ?digitCase : String, ?digitWidth : String, ?ligatureLevel : String, ?typographicCase : String) : Void;
	var alignmentBaseline : String;
	var alpha : Float;
	var baselineShift : Float;
	var breakOpportunity : String;
	var color : UInt;
	var digitCase : String;
	var digitWidth : String;
	var dominantBaseline : String;
	var fontDescription : flash.text.engine.FontDescription;
	var fontSize : Float;
	function getFontMetrics() : flash.text.engine.FontMetrics;
	var kerning : String;
	var ligatureLevel : String;
	var locale : String;
	var textRotation : String;
	var tracking : Float;
	var typographicCase : String;
}
