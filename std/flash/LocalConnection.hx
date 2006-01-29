package flash;

extern class LocalConnection implements Dynamic<String>
{
	function new() : Void;

	function connect(connectionName:String):Bool;
	function send(connectionName:String, methodName:String, args:Dynamic):Bool;
	function close():Void;
	function domain():String;
	function allowDomain(domain:String):Bool;
	function allowInsecureDomain(domain:String):Bool;

	function onStatus(infoObject:Dynamic):Void;
}


