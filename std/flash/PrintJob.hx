package flash;

extern class PrintJob
{
	function start():Bool;
	function addPage(target:Dynamic, printArea:Dynamic, options:Dynamic, frameNum:Float):Bool;
	function send():Void;

	property paperWidth(default,null) : Float;
	property paperHeight(default,null) : Float;
	property pageWidth(default,null) : Float;
	property pageHeight(default,null) : Float;
	property orientation(default,null) : String;
}
