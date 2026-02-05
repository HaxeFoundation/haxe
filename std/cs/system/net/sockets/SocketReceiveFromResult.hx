package cs.system.net.sockets;

/** The result of a  operation. */
@:native("System.Net.Sockets.SocketReceiveFromResult")
extern class SocketReceiveFromResult extends cs.system.ValueType {
	/** The number of bytes received. If the  operation was unsuccessful, then 0. */
	var ReceivedBytes:Int;
	/** The source . */
	var RemoteEndPoint:cs.system.net.EndPoint;
}
