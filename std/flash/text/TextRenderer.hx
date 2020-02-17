package flash.text;

extern class TextRenderer {
	@:flash.property static var antiAliasType(get,set) : AntiAliasType;
	@:flash.property static var displayMode(get,set) : TextDisplayMode;
	@:flash.property static var maxLevel(get,set) : Int;
	private static function get_antiAliasType() : AntiAliasType;
	private static function get_displayMode() : TextDisplayMode;
	private static function get_maxLevel() : Int;
	static function setAdvancedAntiAliasingTable(fontName : String, fontStyle : FontStyle, colorType : TextColorType, advancedAntiAliasingTable : Array<Dynamic>) : Void;
	private static function set_antiAliasType(value : AntiAliasType) : AntiAliasType;
	private static function set_displayMode(value : TextDisplayMode) : TextDisplayMode;
	private static function set_maxLevel(value : Int) : Int;
}
