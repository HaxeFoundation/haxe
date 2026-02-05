package cs.system.net;

/**
 * Represents the method that specifies a local Internet Protocol address and port
 * number for a .
 * @param servicePoint The  associated with the connection to be created.
 * @param remoteEndPoint The remote  that specifies the remote host.
 * @param retryCount The number of times this delegate was called for a specified
 * connection.
 * @return The local  to which the  is bound.
 */
@:native("System.Net.BindIPEndPoint")
extern class BindIPEndPoint extends cs.system.MulticastDelegate {
	function new(func:(servicePoint:cs.system.net.ServicePoint, remoteEndPoint:cs.system.net.IPEndPoint, retryCount:Int)->cs.system.net.IPEndPoint):Void;
	function Invoke(servicePoint:cs.system.net.ServicePoint, remoteEndPoint:cs.system.net.IPEndPoint, retryCount:Int):cs.system.net.IPEndPoint;
}
