package asys.native.net;

typedef SocketOptions = {
	/**
		Whether local addresses can be reused.
	**/
	var reuseAddress:Bool;
	/**
		Whether local ports can be reused.
	**/
	var reusePort:Bool;
	/**
		Enable sending of keep-alive messages on connection-oriented sockets.
	**/
	var keepAlive:Bool;
	/**
		The maximum size of the send buffer in bytes.
	**/
	var sendBuffer:Int;
	/**
		The maximum size of the receive buffer in bytes.
	**/
	var receiveBuffer:Int;
	/**
		Whether UDP sockets are allowed to send packets to a broadcast address.
	**/
	var broadcast:Bool;
	/**
		The outgoing interface for multicast packets.
	**/
	var multicastInterface:String;
	/**
		The multicast loopback policy, which determines whether multicast packets
		sent by the socket also reach receivers in the same host.
		This is the case by default.
	**/
	var multicastLoop:Bool;
	/**
		The time-to-live of outgoing multicast packets.
		This should be a value between 0 (don't leave the interface) and 255.
		The default value is 1 (only the local network is reached).
	**/
	var multicastTtl:Int;
}

enum abstract SocketOptionKind<T>(Int) {
	/**
		Whether local addresses can be reused.
	**/
	var ReuseAddress:SocketOptionKind<Bool>;

	/**
		Whether local ports can be reused.
	**/
	var ReusePort:SocketOptionKind<Bool>;

	/**
		Enable sending of keep-alive messages on connection-oriented sockets.
	**/
	var KeepAlive:SocketOptionKind<Bool>;

	/**
		The maximum size of the send buffer in bytes.
	**/
	var SendBuffer:SocketOptionKind<Int>;

	/**
		The maximum size of the receive buffer in bytes.
	**/
	var ReceiveBuffer:SocketOptionKind<Int>;

	/**
		Whether UDP sockets are allowed to send packets to a broadcast address.
	**/
	var Broadcast:SocketOptionKind<Bool>;

	/**
		The outgoing interface for multicast packets.
	**/
	var MulticastInterface:SocketOptionKind<String>;

	/**
		The multicast loopback policy, which determines whether multicast packets
		sent by the socket also reach receivers in the same host.
		This is the case by default.
	**/
	var MulticastLoop:SocketOptionKind<Bool>;

	/**
		The time-to-live of outgoing multicast packets.
		This should be a value between 0 (don't leave the interface) and 255.
		The default value is 1 (only the local network is reached).
	**/
	var MulticastTtl:SocketOptionKind<Int>;
}