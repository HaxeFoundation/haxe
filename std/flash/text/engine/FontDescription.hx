package flash.text.engine;

@:final extern class FontDescription {
	var cffHinting : CFFHinting;
	var fontLookup : FontLookup;
	var fontName : String;
	var fontPosture : FontPosture;
	var fontWeight : FontWeight;
	var locked : Bool;
	var renderingMode : RenderingMode;
	function new(?fontName : String, ?fontWeight : FontWeight, ?fontPosture : FontPosture, ?fontLookup : FontLookup, ?renderingMode : RenderingMode, ?cffHinting : CFFHinting) : Void;
	function clone() : FontDescription;
	@:require(flash10_1) static function isDeviceFontCompatible(fontName : String, fontWeight : FontWeight, fontPosture : FontPosture) : Bool;
	static function isFontCompatible(fontName : String, fontWeight : FontWeight, fontPosture : FontPosture) : Bool;
}
