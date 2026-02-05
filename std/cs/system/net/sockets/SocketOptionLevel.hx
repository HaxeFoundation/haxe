package cs.system.net.sockets;

/** Defines socket option levels for the  and  methods. */
@:native("System.Net.Sockets.SocketOptionLevel")
extern enum SocketOptionLevel {
	IP;
	IPv6;
	Socket;
	Tcp;
	Udp;
}
