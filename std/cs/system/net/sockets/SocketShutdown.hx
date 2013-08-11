
package cs.system.net.sockets;

@:native("System.Net.Sockets.SocketShutdown")
extern enum SocketShutdown {
	Receive; // Disables a Socket for receiving. This field is constant. 
	Send; // Disables a Socket for sending. This field is constant. 
	Both; // Disables a Socket for both sending and receiving. This field is constant. 
}
