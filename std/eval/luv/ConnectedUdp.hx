package eval.luv;

import eval.luv.SockAddr;

/**
	Connected UDP sockets.

	@see https://aantron.github.io/luv/luv/Luv/UDP/Connected
**/
@:forward
@:using(eval.luv.Handle)
abstract ConnectedUdp(Udp) to Udp to Handle {
	/**
		Removes the peer address assigned to the given socket.
	**/
	extern public function disconnect():Result<Result.NoData>;

	/**
		Retrieves the peer address assigned to the given socket.
	**/
	extern public function getPeerName():Result<SockAddr>;

	/**
		Like `eval.luv.UDP.send`, but the remote address used is the peer address
		assigned to the socket.
	**/
	extern public function send(data:Array<Buffer>, callback:(result:Result<Result.NoData>)->Void):Void;

	/**
		Like `eval.luv.UDP.trySend`, but the remote address used is the peer address
		assigned to the socket.
	**/
	extern public function trySend(data:Array<Buffer>):Result<Result.NoData>;
}