package flash.text;

extern class Font {
	var fontName(default,null) : String;
	var fontStyle(default,null) : FontStyle;
	var fontType(default,null) : FontType;
	function new() : Void;
	function hasGlyphs(str : String) : Bool;
	static function enumerateFonts(enumerateDeviceFonts : Bool = false) : Array<Dynamic>;
	static function registerFont(font : Class<Dynamic>) : Void;
}
