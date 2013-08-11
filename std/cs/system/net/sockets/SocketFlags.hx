
package cs.system.net.sockets;

@:native("System.Net.Sockets.SocketFlags")
extern enum SocketFlags {
	None; // Use no flags for this call. 
	OutOfBand; // Process out-of-band data. 
	Peek; // Peek at the incoming message. 
	DontRoute; // Send without using routing tables. 
	MaxIOVectorLength; // Provides a standard value for the number of WSABUF structures that are used to send and receive data. 
	Truncated; // The message was too large to fit into the specified buffer and was truncated. 
	ControlDataTruncated; // Indicates that the control data did not fit into an internal 64-KB buffer and was truncated. 
	Broadcast; // Indicates a broadcast packet. 
	Multicast; // Indicates a multicast packet. 
	Partial; // Partial send or receive for message. 
}
