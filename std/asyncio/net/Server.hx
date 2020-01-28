package asyncio.net;

import asyncio.net.SocketOption.SocketOptionKind;
import haxe.NoData;
import haxe.Callback;
import haxe.errors.NotImplemented;
import asyncio.filesystem.FilePath;

enum ServerAddress {
	/**
		A network address.
		`host` can be either a host name or IPv4 or IPv6 address.
	**/
	Tcp(host:String, port:Int);
	/**
		An address in the file system pointing to a socket for inter-process
		communications.
	*/
	Ipc(path:FilePath);
}

typedef ServerOptions = {
	/**
		Maximum size of incoming connections queue.
		Default: 0
		TODO: decide on a meaningful default value.
	**/
	var ?backlog:Int;
	/**
		Socket options as described in `asyncio.net.SocketOptions`
	**/
	var ?socketOptions:Array<SocketOption>;
}

class Server {
	/**
		Start a server on specified `address`.

		This methods creates a socket, binds it to `address` and starts listening
		for incoming connections.
		Connections may be accepted with `server.accept` method.

		Maximum size of incoming connections queue is specified by `options.backlog`.
		If the queue is full, any new incoming connection will be rejected.
	**/
	static public function start(address:ServerAddress, ?options:ServerOptions, callback:Callback<Null<Server>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Accept an incoming connection.
	**/
	public function accept(callback:Callback<Null<Socket>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Get the value of a specified socket option.
	**/
	public function getOption<T>(option:SocketOptionKind<T>, callback:Callback<Null<T>>) {
		callback.fail(new NotImplemented());
	}

	/**
		Stop the server.
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplemented());
	}
}