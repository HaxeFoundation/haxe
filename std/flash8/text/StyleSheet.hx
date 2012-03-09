package flash.text;

extern class StyleSheet
{
	function new() : Void;
	function getStyle(name:String):Dynamic;
	function setStyle(name:String,style:Dynamic):Void;
	function clear():Void;
	function getStyleNames():Array<Dynamic>;
	function transform(style:Dynamic):flash.TextFormat;
	function parseCSS(cssText:String):Bool;
	function parse(cssText:String):Bool;
	function load(url:String):Bool;
	dynamic function onLoad(success:Bool):Void;

	private static function __init__() : Void untyped {
		flash.text.StyleSheet = _global["TextField"]["StyleSheet"];
	}

}
