package flash;

extern class PrintJob
{
	function start():Bool;
	function addPage(target:Dynamic, printArea:Dynamic, options:Dynamic, frameNum:Float):Bool;
	function send():Void;

	var paperWidth:Float;
	var paperHeight:Float;
	var pageWidth:Float;
	var pageHeight:Float;
	var orientation:String;
}
