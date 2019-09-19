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

private typedef NativeStream =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Stream;
	#elseif hl
	hl.uv.Stream;
	#elseif neko
	neko.uv.Stream;
	#else
	#error "socket not supported on this platform"
	#end

private typedef NativeSocket =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Socket;
	#elseif hl
	hl.uv.Socket;
	#elseif neko
	neko.uv.Socket;
	#else
	#error "socket not supported on this platform"
	#end

private typedef NativePipe =
	#if doc_gen
	Void;
	#elseif eval
	eval.uv.Pipe;
	#elseif hl
	hl.uv.Pipe;
	#elseif neko
	neko.uv.Pipe;
	#else
	#error "socket not supported on this platform"
	#end

class Server {
	public final closeSignal:Signal<NoData> = new ArraySignal<NoData>();
	public final connectionSignal:Signal<Socket> = new ArraySignal<Socket>();
	public final errorSignal:Signal<Error> = new ArraySignal<Error>();
	public final listeningSignal:Signal<NoData> = new ArraySignal<NoData>();

	public var listening(default, null):Bool;
	public var maxConnections:Int; // TODO

	function get_localAddress():Null<SocketAddress> {
		if (!listening)
			return null;
		return nativeSocket.getSockName();
	}

	public var localAddress(get, never):Null<SocketAddress>;

	public function new(?options:ServerOptions) {}

	// function address():SocketAddress;

	public function close(?callback:Callback<NoData>):Void {
		native.close(Callback.nonNull(callback));
	}

	// function getConnections(callback:Callback<Int>):Void;
	// function listenSocket(socket:Socket, ?backlog:Int, ?listener:Listener<NoData>):Void;
	// function listenServer(server:Server, ?backlog:Int, ?listener:Listener<NoData>):Void;
	// function listenFile(file:sys.io.File, ?backlog:Int, ?listener:Listener<NoData>):Void;
	public function listenIpc(options:ServerListenIpcOptions, ?listener:Listener<Socket>):Void {
		if (listening || listenDefer != null)
			throw "already listening";
		if (listener != null)
			connectionSignal.on(listener);

		nativePipe = new NativePipe(false);
		native = nativePipe.asStream();

		listening = true;
		try {
			// TODO: probably prepend "\\?\pipe\" to the path on Windows
			nativePipe.bindIpc(options.path);
			native.listen(options.backlog == null ? 511 : options.backlog, (err) -> {
				if (err != null)
					return errorSignal.emit(err);
				try {
					var client = @:privateAccess new Socket();
					@:privateAccess client.nativePipe = nativePipe.accept();
					@:privateAccess client.native = @:privateAccess client.nativePipe.asStream();
					@:privateAccess client.connected = true;
					@:privateAccess client.serverSpawn = true;
					connectionSignal.emit(client);
				} catch (e:haxe.Error) {
					errorSignal.emit(e);
				}
			});
			listeningSignal.emit(new NoData());
		} catch (e:haxe.Error) {
			errorSignal.emit(e);
		}
	}

	public function listenTcp(options:ServerListenTcpOptions, ?listener:Listener<Socket>):Void {
		if (listening || listenDefer != null)
			throw "already listening";
		if (listener != null)
			connectionSignal.on(listener);

		if (options.host != null && options.address != null)
			throw "cannot specify both host and address";

		nativeSocket = new NativeSocket();
		native = nativeSocket.asStream();

		// take a copy since we reuse the object asynchronously
		var options = {
			port: options.port,
			host: options.host,
			address: options.address,
			backlog: options.backlog,
			exclusive: options.exclusive,
			ipv6only: options.ipv6only
		};

		function listen(address:Address):Void {
			listenDefer = null;
			listening = true;
			if (options.ipv6only == null)
				options.ipv6only = false;
			try {
				nativeSocket.bindTcp(address, options.port == null ? 0 : options.port, options.ipv6only);
				native.listen(options.backlog == null ? 511 : options.backlog, (err) -> {
					if (err != null)
						return errorSignal.emit(err);
					try {
						var client = @:privateAccess new Socket();
						@:privateAccess client.nativeSocket = nativeSocket.accept();
						@:privateAccess client.native = @:privateAccess client.nativeSocket.asStream();
						@:privateAccess client.connected = true;
						@:privateAccess client.serverSpawn = true;
						connectionSignal.emit(client);
					} catch (e:haxe.Error) {
						errorSignal.emit(e);
					}
				});
				listeningSignal.emit(new NoData());
			} catch (e:haxe.Error) {
				errorSignal.emit(e);
			}
		}

		if (options.address != null) {
			listenDefer = Defer.nextTick(() -> listen(options.address));
			return;
		}
		if (options.host == null)
			options.host = "";
		Dns.lookup(options.host, null, (err, entries) -> {
			if (err != null)
				return errorSignal.emit(err);
			if (entries.length == 0)
				throw "!";
			listen(entries[0]);
		});
	}

	public function ref():Void {
		native.ref();
	}

	public function unref():Void {
		native.unref();
	}

	var native:NativeStream;
	var nativeSocket:NativeSocket;
	var nativePipe:NativePipe;
	var listenDefer:asys.Timer;
}
