package cs.system.net.sockets;

/** Specifies the type of socket that an instance of the  class represents. */
@:native("System.Net.Sockets.SocketType")
extern enum abstract SocketType(Int) {
	var Dgram = 2;
	var Raw = 3;
	var Rdm = 4;
	var Seqpacket = 5;
	var Stream = 1;
	var Unknown = -1;
}
