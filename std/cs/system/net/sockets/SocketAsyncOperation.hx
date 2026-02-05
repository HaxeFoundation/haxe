package cs.system.net.sockets;

/** The type of asynchronous socket operation most recently performed with this context object. */
@:native("System.Net.Sockets.SocketAsyncOperation")
extern enum abstract SocketAsyncOperation(Int) {
	var Accept = 1;
	var Connect = 2;
	var Disconnect = 3;
	var None = 0;
	var Receive = 4;
	var ReceiveFrom = 5;
	var ReceiveMessageFrom = 6;
	var Send = 7;
	var SendPackets = 8;
	var SendTo = 9;
}
