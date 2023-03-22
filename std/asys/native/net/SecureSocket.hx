package asys.native.net;

import haxe.exceptions.NotImplementedException;

typedef SecureSocketOptions = SocketOptions & {
	//TODO
}

/**
	Secure TCP socket.
**/
class SecureSocket extends Socket {
	/**
		Establish a secure connection to specified address.
	**/
	static public function connect(address:SocketAddress, options:SecureSocketOptions, callback:Callback<SecureSocket>) {
		throw new NotImplementedException();
	}

	//TODO
}