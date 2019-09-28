package asys;

import haxe.NoData;
import haxe.async.*;
import asys.net.*;
import asys.net.SocketOptions.SocketConnectTcpOptions;
import asys.net.SocketOptions.SocketConnectIpcOptions;
import asys.net.Server.ServerOptions;
import asys.net.Server.ServerListenTcpOptions;
import asys.net.Server.ServerListenIpcOptions;

enum SocketConnect {
	Tcp(options:SocketConnectTcpOptions);
	Ipc(options:SocketConnectIpcOptions);
}

enum ServerListen {
	Tcp(options:ServerListenTcpOptions);
	Ipc(options:ServerListenIpcOptions);
}

typedef SocketCreationOptions = SocketOptions & {?connect:SocketConnect};

typedef ServerCreationOptions = ServerOptions & {?listen:ServerListen};

/**
	Network utilities.
**/
class Net {
	/**
		Constructs a socket with the given `options`. If `options.connect` is
		given, an appropriate `connect` method is called on the socket. If `cb` is
		given, it is passed to the `connect` method, so it will be called once the
		socket successfully connects or an error occurs during connecting.

		The `options` object is given both to the `Socket` constructor and to the
		`connect` method.
	**/
	public static function createConnection(options:SocketCreationOptions, ?cb:Callback<NoData>):Socket {
		var socket = Socket.create(options);
		if (options != null && options.connect != null)
			switch (options.connect) {
				case Tcp(options):
					socket.connectTcp(options, cb);
				case Ipc(options):
					socket.connectIpc(options, cb);
			}
		return socket;
	}

	/**
		Constructs a server with the given `options`. If `options.listen` is
		given, an appropriate `listen` method is called on the server. If `cb` is
		given, it is passed to the `listen` method, so it will be called for each
		client that connects to the server.

		The `options` object is given both to the `Server` constructor and to the
		`listen` method.
	**/
	public static function createServer(?options:ServerCreationOptions, ?listener:Listener<Socket>):Server {
		var server = new Server(options);
		if (options != null && options.listen != null)
			switch (options.listen) {
				case Tcp(options):
					server.listenTcp(options, listener);
				case Ipc(options):
					server.listenIpc(options, listener);
			}
		return server;
	}
}
