package cs.system.net.sockets;

/** Describes states for a . */
@:native("System.Net.Sockets.SocketInformationOptions")
extern enum abstract SocketInformationOptions(Int) {
	var Connected = 2;
	var Listening = 4;
	var NonBlocking = 1;
	var UseOnlyOverlappedIO = 8;
	@:op(A | B) static function or(lhs:SocketInformationOptions, rhs:SocketInformationOptions):SocketInformationOptions;
	@:op(A & B) static function and(lhs:SocketInformationOptions, rhs:SocketInformationOptions):SocketInformationOptions;
	@:op(A ^ B) static function xor(lhs:SocketInformationOptions, rhs:SocketInformationOptions):SocketInformationOptions;
	@:op(~A) static function complement(value:SocketInformationOptions):SocketInformationOptions;
}
