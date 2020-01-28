package asyncio.net;

import haxe.Callback;
import haxe.errors.NotImplemented;

typedef SecureSocketOptions = {
	var ?socketOptions:Array<SocketOption>;
	//TODO
}

/**
	Secure TCP connections.
**/
class SecureSocket extends Socket {
	/**
		Establish a secure connection to specified address.
	**/
	static public function connect(host:String, port:Int, options:SecureSocketOptions, callback:Callback<Null<SecureSocket>>) {
		callback.fail(new NotImplemented());
	}

	//TODO
}