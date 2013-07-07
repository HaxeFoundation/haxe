package flash;

extern class LoadVars implements Dynamic<String>
{
	var contentType:String;
	var loaded:Bool;

	function new() : Void;

	function addRequestHeader(header:String, headerValue:String):Void;
	// don't allow : function addRequestHeader( headers : Array<String> ) : Void;

	function load(url:String):Bool;
	function send(url:String,target:String,?method:String):Bool;
	function sendAndLoad(url:String,targetObject:Dynamic,?method:String):Bool;
	function getBytesLoaded():Int;
	function getBytesTotal():Int;
	function decode(queryString:String):Void;
	function toString():String;

	dynamic function onLoad(success:Bool):Void;
	dynamic function onData(src:String):Void;

	#if flash8
	dynamic function onHTTPStatus( status : Int ) : Void;
	#end

	// undocumented var _customHeaders:Array<String>;

	private static function __init__() : Void untyped {
		flash.LoadVars = _global["LoadVars"];
	}

}
