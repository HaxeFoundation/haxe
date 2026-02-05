package cs.system.net.sockets;

/** The type of asynchronous socket operation most recently performed with this context object. */
@:native("System.Net.Sockets.SocketAsyncOperation")
extern enum SocketAsyncOperation {
	Accept;
	Connect;
	Disconnect;
	None;
	Receive;
	ReceiveFrom;
	ReceiveMessageFrom;
	Send;
	SendPackets;
	SendTo;
}
