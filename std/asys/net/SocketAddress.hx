package asys.net;

/**
	Reperesents the address of a connected or bound `Socket` object.
**/
enum SocketAddress {
	/**
		Address of a socket connected or bound to an IPv4 or IPv6 address and port.
	**/
	Network(address:Address, port:Int);
	/**
		Filepath of a IPC pipe (Windows named pipe or Unix local domain socket).
	**/
	Unix(path:String);
}
