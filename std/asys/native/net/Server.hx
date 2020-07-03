package asys.native.net;

import asys.native.net.SocketOptions.SocketOptionKind;
import haxe.NoData;
import haxe.Callback;
import haxe.exceptions.NotImplementedException;

typedef ServerOptions = SocketOptions & {
	/**
		Maximum size of incoming connections queue.
		Default: 0
		TODO: decide on a meaningful default value.
	**/
	var ?backlog:Int;
}

/**
	Server socket.
**/
class Server {
	/**
		Local address of this server.
	**/
	public var localAddress(get,never):SocketAddress;
	function get_localAddress():SocketAddress throw new NotImplementedException();

	/**
		Start a server on specified `address`.

		This methods creates a socket, binds it to `address` and starts listening
		for incoming connections.
		Connections may be accepted with `server.accept` method.

		Maximum size of incoming connections queue is specified by `options.backlog`.
		If the queue is full, any new incoming connection will be rejected.
	**/
	static public function open(address:SocketAddress, ?options:ServerOptions, callback:Callback<Null<Server>>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Accept an incoming connection.
	**/
	public function accept(callback:Callback<Null<Socket>>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Get the value of a specified socket option.
	**/
	public function getOption<T>(option:SocketOptionKind<T>, callback:Callback<Null<T>>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Set socket option.
	**/
	public function setOption<T>(option:SocketOptionKind<T>, value:T, callback:Callback<NoData>) {
		callback.fail(new NotImplementedException());
	}

	/**
		Stop the server.
	**/
	public function close(callback:Callback<NoData>) {
		callback.fail(new NotImplementedException());
	}
}