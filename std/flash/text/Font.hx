package flash.text;

extern class Font {
	var fontName(default,never) : String;
	var fontStyle(default,never) : FontStyle;
	var fontType(default,never) : FontType;
	function new() : Void;
	function hasGlyphs(str : String) : Bool;
	static function enumerateFonts(enumerateDeviceFonts : Bool = false) : Array<Font>;
	static function registerFont(font : Class<Dynamic>) : Void;
}
