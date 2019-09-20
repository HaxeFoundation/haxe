package asys.net;

import haxe.Error;
import haxe.NoData;
import haxe.async.*;

typedef ServerOptions = {
	?allowHalfOpen:Bool,
	?pauseOnConnect:Bool
};

typedef ServerListenTcpOptions = {
	?port:Int,
	?host:String,
	?address:Address,
	?backlog:Int,
	?exclusive:Bool,
	?ipv6only:Bool
};

typedef ServerListenIpcOptions = {
	path:String,
	?backlog:Int,
	?exclusive:Bool,
	?readableAll:Bool,
	?writableAll:Bool
};

class Server {
	public final closeSignal:Signal<NoData> = new ArraySignal<NoData>();
	public final connectionSignal:Signal<Socket> = new ArraySignal<Socket>();
	public final errorSignal:Signal<Error> = new ArraySignal<Error>();
	public final listeningSignal:Signal<NoData> = new ArraySignal<NoData>();

	public var listening(default, null):Bool;
	public var maxConnections:Int; // TODO

	extern function get_localAddress():Null<SocketAddress>;

	public var localAddress(get, never):Null<SocketAddress>;

	public function new(?options:ServerOptions) {}

	// function address():SocketAddress;

	extern public function close(?callback:Callback<NoData>):Void;

	// function getConnections(callback:Callback<Int>):Void;
	// function listenSocket(socket:Socket, ?backlog:Int, ?listener:Listener<NoData>):Void;
	// function listenServer(server:Server, ?backlog:Int, ?listener:Listener<NoData>):Void;
	// function listenFile(file:sys.io.File, ?backlog:Int, ?listener:Listener<NoData>):Void;
	extern public function listenIpc(options:ServerListenIpcOptions, ?listener:Listener<Socket>):Void;

	extern public function listenTcp(options:ServerListenTcpOptions, ?listener:Listener<Socket>):Void;

	extern public function ref():Void;

	extern public function unref():Void;

	var listenDefer:asys.Timer;
}
