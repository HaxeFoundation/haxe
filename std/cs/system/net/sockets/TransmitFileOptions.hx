package cs.system.net.sockets;

/** The  enumeration defines values used in file transfer requests. */
@:native("System.Net.Sockets.TransmitFileOptions")
extern enum abstract TransmitFileOptions(Int) {
	var Disconnect = 1;
	var ReuseSocket = 2;
	var UseDefaultWorkerThread = 0;
	var UseKernelApc = 32;
	var UseSystemThread = 16;
	var WriteBehind = 4;
	@:op(A | B) static function or(lhs:TransmitFileOptions, rhs:TransmitFileOptions):TransmitFileOptions;
	@:op(A & B) static function and(lhs:TransmitFileOptions, rhs:TransmitFileOptions):TransmitFileOptions;
	@:op(A ^ B) static function xor(lhs:TransmitFileOptions, rhs:TransmitFileOptions):TransmitFileOptions;
	@:op(~A) static function complement(value:TransmitFileOptions):TransmitFileOptions;
}
