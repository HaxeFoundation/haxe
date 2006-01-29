package flash.text;

extern class StyleSheet
{
	function getStyle(name:String):Dynamic;
	function setStyle(name:String,style:Dynamic):Void;
	function clear():Void;
	function getStyleNames():Array<Dynamic>;
	function transform(style:Dynamic):flash.TextFormat;
	function parseCSS(cssText:String):Bool;
	function parse(cssText:String):Bool;
	function load(url:String):Bool;
	function onLoad(success:Bool):Void;
}
