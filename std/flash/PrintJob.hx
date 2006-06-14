package flash;

extern class PrintJob
{
	function new() : Void;
	function start():Bool;
	function addPage(target:Dynamic, ?printArea:Dynamic, ?options:Dynamic, ?frameNum:Float):Bool;
	function send():Void;

	var paperWidth(default,null) : Float;
	var paperHeight(default,null) : Float;
	var pageWidth(default,null) : Float;
	var pageHeight(default,null) : Float;
	var orientation(default,null) : String;
}
