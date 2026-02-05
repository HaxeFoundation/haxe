package cs.system.net.sockets;

/** Specifies socket send and receive behaviors. */
@:native("System.Net.Sockets.SocketFlags")
extern enum abstract SocketFlags(Int) {
	var Broadcast = 1024;
	var ControlDataTruncated = 512;
	var DontRoute = 4;
	var Multicast = 2048;
	var None = 0;
	var OutOfBand = 1;
	var Partial = 32768;
	var Peek = 2;
	var Truncated = 256;
	@:op(A | B) static function or(lhs:SocketFlags, rhs:SocketFlags):SocketFlags;
	@:op(A & B) static function and(lhs:SocketFlags, rhs:SocketFlags):SocketFlags;
	@:op(A ^ B) static function xor(lhs:SocketFlags, rhs:SocketFlags):SocketFlags;
	@:op(~A) static function complement(value:SocketFlags):SocketFlags;
}
