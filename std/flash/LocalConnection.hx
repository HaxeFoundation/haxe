package flash;

extern class LocalConnection
#if flash_strict
#else true
implements Dynamic
#end
{
	function new() : Void;

	function connect(connectionName:String):Bool;
	function send(connectionName:String, methodName:String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic, ?p6 : Dynamic ):Bool;

	function close():Void;
	function domain():String;
	dynamic function allowDomain(domain:String):Bool;
	dynamic function allowInsecureDomain(domain:String):Bool;
	dynamic function onStatus(infoObject:Dynamic):Void;

	private static function __init__() : Void untyped {
		flash.LocalConnection = _global["LocalConnection"];
	}

}
