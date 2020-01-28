package asyncio.net;

import asyncio.net.SocketOption;
import haxe.Callback;
import haxe.errors.NotImplemented;

typedef SecureServerOptions = {
	var ?socketOptions:Array<SocketOption>;
	//TODO
}

/**
	Secure TCP server.
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
	static public function start(host:String, port:Int, options:SecureServerOptions, callback:Callback<Null<SecureServer>>) {
		callback.fail(new NotImplemented());
	}

	//TODO
}