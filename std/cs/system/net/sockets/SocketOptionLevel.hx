package cs.system.net.sockets;

/** Defines socket option levels for the  and  methods. */
@:native("System.Net.Sockets.SocketOptionLevel")
extern enum abstract SocketOptionLevel(Int) {
	var IP = 0;
	var IPv6 = 41;
	var Socket = 65535;
	var Tcp = 6;
	var Udp = 17;
}
