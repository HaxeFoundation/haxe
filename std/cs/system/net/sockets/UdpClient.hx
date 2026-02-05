package cs.system.net.sockets;

/** Provides User Datagram Protocol (UDP) network services. */
@:native("System.Net.Sockets.UdpClient")
extern class UdpClient {
	/**
	 * Gets or sets a value indicating whether a default remote host has been
	 * established.
	 * @return if a connection is active; otherwise, .
	 */
	var Active(default, default):Bool;
	/**
	 * Gets the amount of data received from the network that is available to read.
	 * @return The number of bytes of data received from the network.
	 */
	var Available(default, never):Int;
	/**
	 * Gets or sets the underlying network .
	 * @return The underlying Network .
	 */
	var Client(default, default):cs.system.net.sockets.Socket;
	/**
	 * Gets or sets a  value that specifies whether the  allows Internet Protocol (IP)
	 * datagrams to be fragmented.
	 * @return if the  allows datagram fragmentation; otherwise, . The default is .
	 */
	var DontFragment(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  may send or receive broadcast
	 * packets.
	 * @return if the  allows broadcast packets; otherwise, . The default is .
	 */
	var EnableBroadcast(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  allows only one client to use
	 * a port.
	 * @return if the  allows only one client to use a specific port; otherwise, . The
	 * default is  for Windows Server 2003 and Windows XP Service Pack 2 and later, and
	 * for all other versions.
	 */
	var ExclusiveAddressUse(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether outgoing multicast packets are
	 * delivered to the sending application.
	 * @return if the  receives outgoing multicast packets; otherwise, .
	 */
	var MulticastLoopback(default, default):Bool;
	/**
	 * Gets or sets a value that specifies the Time to Live (TTL) value of Internet
	 * Protocol (IP) packets sent by the .
	 * @return The TTL value.
	 */
	var Ttl(default, default):cs.Int16;
	@:overload(function():Void {})
	@:overload(function(port:Int):Void {})
	@:overload(function(localEP:cs.system.net.IPEndPoint):Void {})
	@:overload(function(family:cs.system.net.sockets.AddressFamily):Void {})
	@:overload(function(port:Int, family:cs.system.net.sockets.AddressFamily):Void {})
	function new(hostname:String, port:Int):Void;
	/**
	 * Enables or disables Network Address Translation (NAT) traversal on a  instance.
	 * @param allowed A Boolean value that specifies whether to enable or disable NAT
	 * traversal.
	 */
	function AllowNatTraversal(allowed:Bool):Void;
	/**
	 * Receives a datagram from a remote host asynchronously.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param state A user-defined object that contains information about the receive
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  object that references the asynchronous receive.
	 */
	function BeginReceive(requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	@:overload(function(datagram:cs.NativeArray<cs.UInt8>, bytes:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(datagram:cs.NativeArray<cs.UInt8>, bytes:Int, endPoint:cs.system.net.IPEndPoint, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Sends a datagram to a remote host asynchronously. The destination was specified
	 * previously by a call to .
	 * @param datagram A  array that contains the data to be sent.
	 * @param bytes The number of bytes to send.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param state A user-defined object that contains information about the send
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  object that references the asynchronous send.
	 */
	function BeginSend(datagram:cs.NativeArray<cs.UInt8>, bytes:Int, hostname:String, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/** Closes the UDP connection. */
	function Close():Void;
	@:overload(function(endPoint:cs.system.net.IPEndPoint):Void {})
	@:overload(function(addr:cs.system.net.IPAddress, port:Int):Void {})
	/**
	 * Establishes a default remote host using the specified IP address and port
	 * number.
	 * @param addr The  of the remote host to which you intend to send data.
	 * @param port The port number to which you intend send data.
	 */
	function Connect(hostname:String, port:Int):Void;
	/** Releases the managed and unmanaged resources used by the . */
	function Dispose():Void;
	@:overload(function(multicastAddr:cs.system.net.IPAddress):Void {})
	/**
	 * Leaves a multicast group.
	 * @param multicastAddr The  of the multicast group to leave.
	 */
	function DropMulticastGroup(multicastAddr:cs.system.net.IPAddress, ifindex:Int):Void;
	/**
	 * Ends a pending asynchronous receive.
	 * @param asyncResult An  object returned by a call to .
	 * @param remoteEP The specified remote endpoint.
	 * @return If successful, an array of bytes that contains datagram data.
	 */
	function EndReceive(asyncResult:cs.system.IAsyncResult, remoteEP:cs.Ref<cs.system.net.IPEndPoint>):cs.NativeArray<cs.UInt8>;
	/**
	 * Ends a pending asynchronous send.
	 * @param asyncResult An  object returned by a call to .
	 * @return If successful, the number of bytes sent to the .
	 */
	function EndSend(asyncResult:cs.system.IAsyncResult):Int;
	@:overload(function(multicastAddr:cs.system.net.IPAddress):Void {})
	@:overload(function(ifindex:Int, multicastAddr:cs.system.net.IPAddress):Void {})
	@:overload(function(multicastAddr:cs.system.net.IPAddress, timeToLive:Int):Void {})
	/**
	 * Adds a  to a multicast group.
	 * @param ifindex The interface index associated with the local IP address on which
	 * to join the multicast group.
	 * @param multicastAddr The multicast  of the group you want to join.
	 */
	function JoinMulticastGroup(multicastAddr:cs.system.net.IPAddress, localAddress:cs.system.net.IPAddress):Void;
	/**
	 * Returns a UDP datagram that was sent by a remote host.
	 * @param remoteEP An  that represents the remote host from which the data was
	 * sent.
	 * @return An array of type  that contains datagram data.
	 */
	function Receive(remoteEP:cs.Ref<cs.system.net.IPEndPoint>):cs.NativeArray<cs.UInt8>;
	/**
	 * Returns a UDP datagram asynchronously that was sent by a remote host.
	 * @return The task object representing the asynchronous operation.
	 */
	function ReceiveAsync():cs.system.threading.tasks.Task_1<cs.system.net.sockets.UdpReceiveResult>;
	@:overload(function(dgram:cs.NativeArray<cs.UInt8>, bytes:Int):Int {})
	@:overload(function(dgram:cs.NativeArray<cs.UInt8>, bytes:Int, endPoint:cs.system.net.IPEndPoint):Int {})
	/**
	 * Sends a UDP datagram to a remote host.
	 * @param dgram An array of type  that specifies the UDP datagram that you intend
	 * to send represented as an array of bytes.
	 * @param bytes The number of bytes in the datagram.
	 * @return The number of bytes sent.
	 */
	function Send(dgram:cs.NativeArray<cs.UInt8>, bytes:Int, hostname:String, port:Int):Int;
	@:overload(function(datagram:cs.NativeArray<cs.UInt8>, bytes:Int):cs.system.threading.tasks.Task_1<Int> {})
	@:overload(function(datagram:cs.NativeArray<cs.UInt8>, bytes:Int, endPoint:cs.system.net.IPEndPoint):cs.system.threading.tasks.Task_1<Int> {})
	/**
	 * Sends a UDP datagram asynchronously to a remote host.
	 * @param datagram An array of type  that specifies the UDP datagram that you
	 * intend to send represented as an array of bytes.
	 * @param bytes The number of bytes in the datagram.
	 * @return Returns .
	 */
	function SendAsync(datagram:cs.NativeArray<cs.UInt8>, bytes:Int, hostname:String, port:Int):cs.system.threading.tasks.Task_1<Int>;
}
