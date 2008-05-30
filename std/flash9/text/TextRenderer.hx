package flash.text;

extern class TextRenderer {
	static var antiAliasType : AntiAliasType;
	static var displayMode : TextDisplayMode;
	static var maxLevel : Int;
	static function setAdvancedAntiAliasingTable(fontName : String, fontStyle : FontStyle, colorType : TextColorType, advancedAntiAliasingTable : Array<Dynamic>) : Void;
}
