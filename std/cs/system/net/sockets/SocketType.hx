package cs.system.net.sockets;

/** Specifies the type of socket that an instance of the  class represents. */
@:native("System.Net.Sockets.SocketType")
extern enum SocketType {
	Dgram;
	Raw;
	Rdm;
	Seqpacket;
	Stream;
	Unknown;
}
