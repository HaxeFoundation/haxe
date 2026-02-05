package cs.system.net.sockets;

/** Provides client connections for TCP network services. */
@:native("System.Net.Sockets.TcpClient")
extern class TcpClient {
	/**
	 * Gets or sets a value that indicates whether a connection has been made.
	 * @return if the connection has been made; otherwise, .
	 */
	var Active(default, default):Bool;
	/**
	 * Gets the amount of data that has been received from the network and is available
	 * to be read.
	 * @return The number of bytes of data received from the network and available to
	 * be read.
	 */
	var Available(default, never):Int;
	/**
	 * Gets or sets the underlying .
	 * @return The underlying network .
	 */
	var Client(default, default):cs.system.net.sockets.Socket;
	/**
	 * Gets a value indicating whether the underlying  for a  is connected to a remote
	 * host.
	 * @return if the  socket was connected to a remote resource as of the most recent
	 * operation; otherwise, .
	 */
	var Connected(default, never):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  allows only one client to use
	 * a port.
	 * @return if the  allows only one client to use a specific port; otherwise, . The
	 * default is  for Windows Server 2003 and Windows XP Service Pack 2 and later, and
	 * for all other versions.
	 */
	var ExclusiveAddressUse(default, default):Bool;
	/**
	 * Gets or sets information about the linger state of the associated socket.
	 * @return A . By default, lingering is disabled.
	 */
	var LingerState(default, default):cs.system.net.sockets.LingerOption;
	/**
	 * Gets or sets a value that disables a delay when send or receive buffers are not
	 * full.
	 * @return if the delay is disabled; otherwise, . The default value is .
	 */
	var NoDelay(default, default):Bool;
	/**
	 * Gets or sets the size of the receive buffer.
	 * @return The size of the receive buffer, in bytes. The default value is 8192
	 * bytes.
	 */
	var ReceiveBufferSize(default, default):Int;
	/**
	 * Gets or sets the amount of time a  will wait to receive data once a read
	 * operation is initiated.
	 * @return The time-out value of the connection in milliseconds. The default value
	 * is 0.
	 */
	var ReceiveTimeout(default, default):Int;
	/**
	 * Gets or sets the size of the send buffer.
	 * @return The size of the send buffer, in bytes. The default value is 8192 bytes.
	 */
	var SendBufferSize(default, default):Int;
	/**
	 * Gets or sets the amount of time a  will wait for a send operation to complete
	 * successfully.
	 * @return The send time-out value, in milliseconds. The default is 0.
	 */
	var SendTimeout(default, default):Int;
	@:overload(function():Void {})
	@:overload(function(localEP:cs.system.net.IPEndPoint):Void {})
	@:overload(function(family:cs.system.net.sockets.AddressFamily):Void {})
	function new(hostname:String, port:Int):Void;
	@:overload(function(address:cs.system.net.IPAddress, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(addresses:cs.NativeArray<cs.system.net.IPAddress>, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Begins an asynchronous request for a remote host connection. The remote host is
	 * specified by an  and a port number ().
	 * @param address The  of the remote host.
	 * @param port The port number of the remote host.
	 * @param requestCallback An  delegate that references the method to invoke when
	 * the operation is complete.
	 * @param state A user-defined object that contains information about the connect
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  object that references the asynchronous connection.
	 */
	function BeginConnect(host:String, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/** Disposes this  instance and requests that the underlying TCP connection be closed. */
	function Close():Void;
	@:overload(function(remoteEP:cs.system.net.IPEndPoint):Void {})
	@:overload(function(address:cs.system.net.IPAddress, port:Int):Void {})
	@:overload(function(ipAddresses:cs.NativeArray<cs.system.net.IPAddress>, port:Int):Void {})
	/**
	 * Connects the client to a remote TCP host using the specified IP address and port
	 * number.
	 * @param address The  of the host to which you intend to connect.
	 * @param port The port number to which you intend to connect.
	 */
	function Connect(hostname:String, port:Int):Void;
	@:overload(function(address:cs.system.net.IPAddress, port:Int):cs.system.threading.tasks.Task {})
	@:overload(function(addresses:cs.NativeArray<cs.system.net.IPAddress>, port:Int):cs.system.threading.tasks.Task {})
	/**
	 * Connects the client to a remote TCP host using the specified IP address and port
	 * number as an asynchronous operation.
	 * @param address The  of the host to which you intend to connect.
	 * @param port The port number to which you intend to connect.
	 * @return The task object representing the asynchronous operation.
	 */
	function ConnectAsync(host:String, port:Int):cs.system.threading.tasks.Task;
	/** Releases the managed and unmanaged resources used by the . */
	function Dispose():Void;
	/**
	 * Ends a pending asynchronous connection attempt.
	 * @param asyncResult An  object returned by a call to .
	 */
	function EndConnect(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Returns the  used to send and receive data.
	 * @return The underlying .
	 */
	function GetStream():cs.system.net.sockets.NetworkStream;
}
