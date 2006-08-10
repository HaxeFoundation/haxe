package flash.text;

extern class TextRenderer {
	function new() : Void;
	static var antiAliasType : String;
	static var displayMode : String;
	static var maxLevel : Int;
	static function setAdvancedAntiAliasingTable(fontName : String, fontStyle : String, colorType : String, advancedAntiAliasingTable : Array<Dynamic>) : Void;
}
