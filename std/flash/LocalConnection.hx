package flash;

extern class LocalConnection
#if flash_strict
#else true
implements Dynamic<String>
#end
{
	function new() : Void;

	function connect(connectionName:String):Bool;
	function send(connectionName:String, methodName:String, p1 : Dynamic, p2 : Dynamic, p3 : Dynamic, p4 : Dynamic, p5 : Dynamic, p6 : Dynamic ):Bool;

	function close():Void;
	function domain():String;
	function allowDomain(domain:String):Bool;
	function allowInsecureDomain(domain:String):Bool;

	function onStatus(infoObject:Dynamic):Void;
}
