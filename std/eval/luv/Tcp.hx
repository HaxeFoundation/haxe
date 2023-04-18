package eval.luv;

import haxe.ds.Option;
import eval.luv.SockAddr;

/**
	TCP sockets.

	@see https://aantron.github.io/luv/luv/Luv/TCP
**/
@:using(eval.luv.Handle)
@:using(eval.luv.Stream)
@:coreType abstract Tcp to Handle to Stream to Stream.TStream<Tcp> to Handle.SocketHandle {
	/**
		Allocates and initializes a TCP stream.

		The stream is not yet connected or listening.

		The handle should be cleaned up with `eval.luv.Handle.close` when no longer needed.
	**/
	static public function init(loop:Loop, ?domain:AddressFamily):Result<Tcp>;

	/**
		Sets TCP_NODELAY.
	**/
	public function noDelay(enable:Bool):Result<Result.NoData>;

	/**
		Sets the TCP keepalive.
	**/
	public function keepAlive(value:Option<Int>):Result<Result.NoData>;

	/**
		Sets simultaneous accept.
	**/
	public function simultaneousAccepts(value:Bool):Result<Result.NoData>;

	/**
		Assigns an address to the TCP socket.
	**/
	public function bind(addr:SockAddr, ipv6Only:Bool = false):Result<Result.NoData>;

	/**
		Retrieves the address assigned to the TCP socket.
	**/
	public function getSockName():Result<SockAddr>;

	/**
		Retrieves the address of the TCP socket's peer.
	**/
	public function getPeerName():Result<SockAddr>;

	/**
		Connects to a host.
	**/
	public function connect(addr:SockAddr, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Resets the connection.
	**/
	public function closeReset(callback:(result:Result<Result.NoData>)->Void):Void;
}