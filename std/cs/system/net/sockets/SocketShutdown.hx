package cs.system.net.sockets;

/** Defines constants that are used by the  method. */
@:native("System.Net.Sockets.SocketShutdown")
extern enum abstract SocketShutdown(Int) {
	var Both = 2;
	var Receive = 0;
	var Send = 1;
}
