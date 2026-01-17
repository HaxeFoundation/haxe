package cs.system.net.sockets;

@:native("System.Net.Sockets.Socket")
extern class Socket {
	function new(addressFamily:Int, socketType:Int, protocolType:Int):Void;
	function Connect(host:String, port:Int):Void;
	function Close():Void;
	function Bind(endpoint:Dynamic):Void;
	function Listen(backlog:Int):Void;
	function Accept():Socket;
	function Shutdown(how:Int):Void;
	var RemoteEndPoint(default, never):Dynamic;
	var LocalEndPoint(default, never):Dynamic;
	var ReceiveTimeout(default, default):Int;
	var SendTimeout(default, default):Int;
	var Blocking(default, default):Bool;
	var NoDelay(default, default):Bool;
}
