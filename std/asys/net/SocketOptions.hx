package asys.net;

/**
	See `Socket.create`.
**/
typedef SocketOptions = {
	// ?file:asys.io.File, // fd in Node
	?allowHalfOpen:Bool,
	?readable:Bool,
	?writable:Bool
};

/**
	See `Socket.connectTcp`.
**/
typedef SocketConnectTcpOptions = {
	port:Int,
	?host:String,
	?address:Address,
	?localAddress:Address,
	?localPort:Int,
	?family:IpFamily
};

/**
	See `Socket.connectIpc`.
**/
typedef SocketConnectIpcOptions = {
	path:String
};

/**
	See `UdpSocket.create`.
**/
typedef UdpSocketOptions = {
	?reuseAddr:Bool,
	?ipv6Only:Bool,
	?recvBufferSize:Int,
	?sendBufferSize:Int,
	// ?lookup:DnsLookupFunction
};
