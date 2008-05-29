package flash.text.engine;

extern class FontDescription {
	function new(?fontName : String, ?fontWeight : String, ?fontPosture : String, ?fontLookup : String, ?antiAliasType : String, ?gridFitType : String, ?sharpness : Float, ?thickness : Float) : Void;
	var antiAliasType : String;
	var fontLookup : String;
	var fontName : String;
	var fontPosture : String;
	var fontWeight : String;
	var gridFitType : String;
	var sharpness : Float;
	var thickness : Float;
}
