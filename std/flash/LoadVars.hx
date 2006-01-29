package flash;

extern class LoadVars implements Dynamic<String>
{
	var contentType:String;
	var loaded:Bool;
	var _customHeaders:Array<String>;

	function new() : Void;

	function addRequestHeader(header:Dynamic, headerValue:String):Void;
	function load(url:String):Bool;
	function send(url:String,target:String,method:String):Bool;
	function sendAndLoad(url:String,target:String,method:String):Bool;
	function getBytesLoaded():Float;
	function getBytesTotal():Float;
	function decode(queryString:String):Void;
	function toString():String;

	function onLoad(success:Bool):Void;
	function onData(src:String):Void;

}
