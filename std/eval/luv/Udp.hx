package eval.luv;

import haxe.ds.Option;
import eval.luv.SockAddr;

enum abstract UdpMembership(Int) {
	var LEAVE_GROUP = 0;
	var JOIN_GROUP = 1;
}

enum abstract RecvFlag(Int) {
	var PARTIAL = 0;
	var MMSG_CHUNK = 1;
	var MMSG_FREE = 2;
}

/**
	UDP sockets.

	@see https://aantron.github.io/luv/luv/Luv/UDP
**/
@:using(eval.luv.Handle)
@:coreType abstract Udp to Handle to Handle.SocketHandle {
	/**
		Allocates and initializes a UDP socket.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop, ?domain:AddressFamily, recvmmsg:Bool = false):Result<Udp>;

	/**
		Assigns an address to the UDP socket.
	**/
	public function bind(addr:SockAddr, ipv6Only:Bool = false, reuseAddr:Bool = false):Result<Result.NoData>;

	/**
		Assigns a peer address to the socket.
	**/
	public function connect(addr:SockAddr):Result<ConnectedUdp>;

	/**
		Retrieves the address assigned to the UDP socket.
	**/
	public function getSockName():Result<SockAddr>;

	/**
		Sets multicast group membership.
	**/
	public function setMembership(group:String, interfaceName:String, membership:UdpMembership):Result<Result.NoData>;

	/**
		Sets source-specific multicast group membership.
	**/
	public function setSourceMembership(group:String, interfaceName:String, source:String, membership:UdpMembership):Result<Result.NoData>;

	/**
		Set multicast loopback.
	**/
	public function setMulticastLoop(value:Bool):Result<Result.NoData>;

	/**
		Set multicast TTL.
	**/
	public function setMulticastTtl(value:Int):Result<Result.NoData>;

	/**
		Sets the interface to be used for multicast.
	**/
	public function setMulticastInterface(value:Int):Result<Result.NoData>;

	/**
		Sets broadcast.
	**/
	public function setBroadcast(value:Bool):Result<Result.NoData>;

	/**
		Sets the TTL.
	**/
	public function setTtl(value:Int):Result<Result.NoData>;

	/**
		Sends a datagram.

		For connected UDP sockets, see `eval.luv.UDP.Connected.send`.
	**/
	public function send(data:Array<Buffer>, addr:SockAddr, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Like `eval.luv.UDP.send`, but only attempts to send the datagram immediately.
	**/
	public function trySend(data:Array<Buffer>, addr:SockAddr):Result<Result.NoData>;

	/**
		Calls `callback` whenever a datagram is received on the UDP socket.

		@see https://aantron.github.io/luv/luv/Luv/UDP/index.html#val-recv_start
	**/
	public function recvStart(callback:(result:Result<{data:Buffer, addr:Option<SockAddr>, flags:Array<RecvFlag>}>, ?allocate:(size:Int)->Buffer)->Void):Void;

	/**
		Stops the callback provided to `eval.luv.UDP.recvStart`.
	**/
	public function recvStop():Result<Result.NoData>;

	/**
		Evaluates to true if and only if the UDP was created with `recvmmsg = true`
		and the platform supports recvmmsg(2).
	**/
	public function usingRecvmmsg():Bool;

	/**
		Number of bytes queued for sending. This field strictly shows how much
		information is currently queued.
	**/
	public function getSendQueueSize():Int;

	/**
		Number of send requests currently in the queue awaiting to be processed.
	**/
	public function getSendQueueCount():Int;
}