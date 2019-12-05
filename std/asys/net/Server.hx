package asys.net;

import asys.uv.UVError;
import haxe.NoData;
import haxe.signals.Signal;
import haxe.signals.ArraySignal;

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
	public var closeSignal(get,never):Signal<NoData>;
	final _closeSignal = new ArraySignal<NoData>();
	inline function get_closeSignal():Signal<NoData>
		return _closeSignal;

	public var connectionSignal(get,never):Signal<Socket>;
	final _connectionSignal = new ArraySignal<Socket>();
	inline function get_connectionSignal():Signal<Socket>
		return _connectionSignal;

	public var errorSignal(get,never):Signal<UVError>;
	final _errorSignal = new ArraySignal<UVError>();
	inline function get_errorSignal():Signal<UVError>
		return _errorSignal;

	public var listeningSignal(get,never):Signal<NoData>;
	final _listeningSignal = new ArraySignal<NoData>();
	inline function get_listeningSignal():Signal<NoData>
		return _listeningSignal;


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
