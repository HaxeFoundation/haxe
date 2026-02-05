package cs.system.net.sockets;

/** The result of a  operation. */
@:native("System.Net.Sockets.SocketReceiveMessageFromResult")
extern class SocketReceiveMessageFromResult extends cs.system.ValueType {
	/** An  holding address and interface information. */
	var PacketInformation:cs.system.net.sockets.IPPacketInformation;
	/** The number of bytes received. If the  operation is unsuccessful, this value will be 0. */
	var ReceivedBytes:Int;
	/** The source . */
	var RemoteEndPoint:cs.system.net.EndPoint;
	/** A bitwise combination of the  values for the received packet. */
	var SocketFlags:cs.system.net.sockets.SocketFlags;
}
