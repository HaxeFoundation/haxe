package flash.text;

extern class Font {
	function new() : Void;
	var fontName(default,null) : String;
	var fontStyle(default,null) : String;
	var fontType(default,null) : String;
	function hasGlyphs(str : String) : Bool;
	static function enumerateFonts(?enumerateDeviceFonts : Bool) : Array<Dynamic>;
	static function registerFont(font : Class<Dynamic>) : Void;
}
