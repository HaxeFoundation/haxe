package flash.text;

extern class Font {
	@:flash.property var fontName(get,never) : String;
	@:flash.property var fontStyle(get,never) : FontStyle;
	@:flash.property var fontType(get,never) : FontType;
	function new() : Void;
	private function get_fontName() : String;
	private function get_fontStyle() : FontStyle;
	private function get_fontType() : FontType;
	function hasGlyphs(str : String) : Bool;
	static function enumerateFonts(enumerateDeviceFonts : Bool = false) : Array<Font>;
	static function registerFont(font : Class<Dynamic>) : Void;
}
