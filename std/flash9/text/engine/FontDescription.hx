package flash.text.engine;

extern class FontDescription {
	function new(?fontName : String, ?fontWeight : flash.text.engine.FontWeight, ?fontPosture : flash.text.engine.FontPosture, ?fontLookup : flash.text.engine.FontLookup, ?renderingMode : flash.text.engine.RenderingMode, ?cffHinting : flash.text.engine.CFFHinting) : Void;
	var cffHinting : flash.text.engine.CFFHinting;
	function clone() : flash.text.engine.FontDescription;
	var fontLookup : flash.text.engine.FontLookup;
	var fontName : String;
	var fontPosture : flash.text.engine.FontPosture;
	var fontWeight : flash.text.engine.FontWeight;
	function isFontCompatible(fontName : String, fontWeight : flash.text.engine.FontWeight, fontPosture : flash.text.engine.FontPosture) : Bool;
	var locked : Bool;
	var renderingMode : flash.text.engine.RenderingMode;
}
