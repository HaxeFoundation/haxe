package asys.native.net;

import haxe.Callback;
import haxe.exceptions.NotImplementedException;

typedef SecureServerOptions = SocketOptions & {
	//TODO
}

/**
	Secure TCP server socket.
**/
class SecureServer extends Server {
	/**
		Start a secure server on specified address.

		This methods creates a secure socket, binds it to `address` and starts
		listening for incoming connections.
		Connections may be accepted with `server.accept` method.

		Maximum size of incoming connections queue is specified by `backlog`.
		If the queue is full, any new incoming connection will be rejected.
	**/
	static public function open(address:SocketAddress, options:SecureServerOptions, callback:Callback<Null<SecureServer>>) {
		callback.fail(new NotImplementedException());
	}

	//TODO
}