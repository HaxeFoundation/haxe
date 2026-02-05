package cs.system.net.sockets;

/** Implements the Berkeley sockets interface. */
@:native("System.Net.Sockets.Socket")
extern class Socket {
	/**
	 * Indicates whether the underlying operating system and network adaptors support
	 * Internet Protocol version 4 (IPv4).
	 * @return if the operating system and network adaptors support the IPv4 protocol;
	 * otherwise, .
	 */
	static var OSSupportsIPv4(default, never):Bool;
	/**
	 * Indicates whether the underlying operating system and network adaptors support
	 * Internet Protocol version 6 (IPv6).
	 * @return if the operating system and network adaptors support the IPv6 protocol;
	 * otherwise, .
	 */
	static var OSSupportsIPv6(default, never):Bool;
	/**
	 * Gets a value indicating whether IPv4 support is available and enabled on the
	 * current host.
	 * @return if the current host supports the IPv4 protocol; otherwise, .
	 */
	static var SupportsIPv4(default, never):Bool;
	/**
	 * Gets a value that indicates whether the Framework supports IPv6 for certain
	 * obsolete  members.
	 * @return if the Framework supports IPv6 for certain obsolete  methods; otherwise,
	 * .
	 */
	static var SupportsIPv6(default, never):Bool;
	/**
	 * Gets the address family of the .
	 * @return One of the  values.
	 */
	var AddressFamily(default, never):cs.system.net.sockets.AddressFamily;
	/**
	 * Gets the amount of data that has been received from the network and is available
	 * to be read.
	 * @return The number of bytes of data received from the network and available to
	 * be read.
	 */
	var Available(default, never):Int;
	/**
	 * Gets or sets a value that indicates whether the  is in blocking mode.
	 * @return if the  will block; otherwise, . The default is .
	 */
	var Blocking(default, default):Bool;
	/**
	 * Gets a value that indicates whether a  is connected to a remote host as of the
	 * last  or  operation.
	 * @return if the  was connected to a remote resource as of the most recent
	 * operation; otherwise, .
	 */
	var Connected(default, never):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  allows Internet Protocol (IP)
	 * datagrams to be fragmented.
	 * @return if the  allows datagram fragmentation; otherwise, . The default is .
	 */
	var DontFragment(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  is a dual-mode socket used for
	 * both IPv4 and IPv6.
	 * @return if the  is a  dual-mode socket; otherwise, . The default is .
	 */
	var DualMode(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  can send or receive broadcast
	 * packets.
	 * @return if the  allows broadcast packets; otherwise, . The default is .
	 */
	var EnableBroadcast(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  allows only one process to
	 * bind to a port.
	 * @return if the  allows only one socket to bind to a specific port; otherwise, .
	 * The default is  for Windows Server 2003 and Windows XP Service Pack 2, and  for
	 * all other versions.
	 */
	var ExclusiveAddressUse(default, default):Bool;
	/**
	 * Gets the operating system handle for the .
	 * @return An  that represents the operating system handle for the .
	 */
	var Handle(default, never):cs.system.IntPtr;
	/**
	 * Gets a value that indicates whether the  is bound to a specific local port.
	 * @return if the  is bound to a local port; otherwise, .
	 */
	var IsBound(default, never):Bool;
	/**
	 * Gets or sets a value that specifies whether the  will delay closing a socket in
	 * an attempt to send all pending data.
	 * @return A  that specifies how to linger while closing a socket.
	 */
	var LingerState(default, default):cs.system.net.sockets.LingerOption;
	/**
	 * Gets the local endpoint.
	 * @return The  that the  is using for communications.
	 */
	var LocalEndPoint(default, never):cs.system.net.EndPoint;
	/**
	 * Gets or sets a value that specifies whether outgoing multicast packets are
	 * delivered to the sending application.
	 * @return if the  receives outgoing multicast packets; otherwise, .
	 */
	var MulticastLoopback(default, default):Bool;
	/**
	 * Gets or sets a  value that specifies whether the stream  is using the Nagle
	 * algorithm.
	 * @return if the  uses the Nagle algorithm; otherwise, . The default is .
	 */
	var NoDelay(default, default):Bool;
	/**
	 * Gets the protocol type of the .
	 * @return One of the  values.
	 */
	var ProtocolType(default, never):cs.system.net.sockets.ProtocolType;
	/**
	 * Gets or sets a value that specifies the size of the receive buffer of the .
	 * @return An  that contains the size, in bytes, of the receive buffer. The default
	 * is 8192.
	 */
	var ReceiveBufferSize(default, default):Int;
	/**
	 * Gets or sets a value that specifies the amount of time after which a synchronous
	 * call will time out.
	 * @return The time-out value, in milliseconds. The default value is 0, which
	 * indicates an infinite time-out period. Specifying -1 also indicates an infinite
	 * time-out period.
	 */
	var ReceiveTimeout(default, default):Int;
	/**
	 * Gets the remote endpoint.
	 * @return The  with which the  is communicating.
	 */
	var RemoteEndPoint(default, never):cs.system.net.EndPoint;
	/**
	 * Gets or sets a value that specifies the size of the send buffer of the .
	 * @return An  that contains the size, in bytes, of the send buffer. The default is
	 * 8192.
	 */
	var SendBufferSize(default, default):Int;
	/**
	 * Gets or sets a value that specifies the amount of time after which a synchronous
	 * call will time out.
	 * @return The time-out value, in milliseconds. If you set the property with a
	 * value between 1 and 499, the value will be changed to 500. The default value is
	 * 0, which indicates an infinite time-out period. Specifying -1 also indicates an
	 * infinite time-out period.
	 */
	var SendTimeout(default, default):Int;
	/**
	 * Gets the type of the .
	 * @return One of the  values.
	 */
	var SocketType(default, never):cs.system.net.sockets.SocketType;
	/**
	 * Gets or sets a value that specifies the Time To Live (TTL) value of Internet
	 * Protocol (IP) packets sent by the .
	 * @return The TTL value.
	 */
	var Ttl(default, default):cs.Int16;
	/**
	 * Specifies whether the socket should only use Overlapped I/O mode.
	 * @return if the  uses only overlapped I/O; otherwise, . The default is .
	 */
	var UseOnlyOverlappedIO(default, default):Bool;
	@:overload(function(socketInformation:cs.system.net.sockets.SocketInformation):Void {})
	@:overload(function(socketType:cs.system.net.sockets.SocketType, protocolType:cs.system.net.sockets.ProtocolType):Void {})
	function new(addressFamily:cs.system.net.sockets.AddressFamily, socketType:cs.system.net.sockets.SocketType, protocolType:cs.system.net.sockets.ProtocolType):Void;
	/**
	 * Cancels an asynchronous request for a remote host connection.
	 * @param e The  object used to request the connection to the remote host by
	 * calling one of the  methods.
	 */
	static function CancelConnectAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Void;
	/**
	 * Begins an asynchronous request for a connection to a remote host.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	static function ConnectAsync(socketType:cs.system.net.sockets.SocketType, protocolType:cs.system.net.sockets.ProtocolType, e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	/**
	 * Determines the status of one or more sockets.
	 * @param checkRead An  of  instances to check for readability.
	 * @param checkWrite An  of  instances to check for writability.
	 * @param checkError An  of  instances to check for errors.
	 * @param microSeconds The time-out value, in microseconds. A -1 value indicates an
	 * infinite time-out.
	 */
	static function Select(checkRead:cs.system.collections.IList, checkWrite:cs.system.collections.IList, checkError:cs.system.collections.IList, microSeconds:Int):Void;
	/**
	 * Creates a new  for a newly created connection.
	 * @return A  for a newly created connection.
	 */
	function Accept():cs.system.net.sockets.Socket;
	/**
	 * Begins an asynchronous operation to accept an incoming connection attempt.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. The  event on the  parameter will not be raised and the  object
	 * passed as a parameter may be examined immediately after the method call returns
	 * to retrieve the result of the operation.
	 */
	function AcceptAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	@:overload(function(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(receiveSize:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Begins an asynchronous operation to accept an incoming connection attempt.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous  creation.
	 */
	function BeginAccept(acceptSocket:cs.system.net.sockets.Socket, receiveSize:Int, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	@:overload(function(remoteEP:cs.system.net.EndPoint, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(address:cs.system.net.IPAddress, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(addresses:cs.NativeArray<cs.system.net.IPAddress>, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Begins an asynchronous request for a remote host connection.
	 * @param remoteEP An  that represents the remote host.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous connection.
	 */
	function BeginConnect(host:String, port:Int, requestCallback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous request to disconnect from a remote endpoint.
	 * @param reuseSocket if this socket can be reused after the connection is closed;
	 * otherwise, .
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  object that references the asynchronous operation.
	 */
	function BeginDisconnect(reuseSocket:Bool, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Begins to asynchronously receive data from a connected .
	 * @param buffer An array of type  that is the storage location for the received
	 * data.
	 * @param offset The zero-based position in the  parameter at which to store the
	 * received data.
	 * @param size The number of bytes to receive.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param callback An  delegate that references the method to invoke when the
	 * operation is complete.
	 * @param state A user-defined object that contains information about the receive
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  that references the asynchronous read.
	 */
	function BeginReceive(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins to asynchronously receive data from a specified network device.
	 * @param buffer An array of type  that is the storage location for the received
	 * data.
	 * @param offset The zero-based position in the  parameter at which to store the
	 * data.
	 * @param size The number of bytes to receive.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP An  that represents the source of the data.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous read.
	 */
	function BeginReceiveFrom(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.Ref<cs.system.net.EndPoint>, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins to asynchronously receive the specified number of bytes of data into the
	 * specified location of the data buffer, using the specified , and stores the
	 * endpoint and packet information.
	 * @param buffer An array of type  that is the storage location for the received
	 * data.
	 * @param offset The zero-based position in the  parameter at which to store the
	 * data.
	 * @param size The number of bytes to receive.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP An  that represents the source of the data.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous read.
	 */
	function BeginReceiveMessageFrom(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.Ref<cs.system.net.EndPoint>, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Sends data asynchronously to a connected .
	 * @param buffer An array of type  that contains the data to send.
	 * @param offset The zero-based position in the  parameter at which to begin
	 * sending data.
	 * @param size The number of bytes to send.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous send.
	 */
	function BeginSend(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	@:overload(function(fileName:String, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult {})
	/**
	 * Sends the file  to a connected  object using the  flag.
	 * @param fileName A string that contains the path and name of the file to send.
	 * This parameter can be .
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  object that represents the asynchronous send.
	 */
	function BeginSendFile(fileName:String, preBuffer:cs.NativeArray<cs.UInt8>, postBuffer:cs.NativeArray<cs.UInt8>, flags:cs.system.net.sockets.TransmitFileOptions, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Sends data asynchronously to a specific remote host.
	 * @param buffer An array of type  that contains the data to send.
	 * @param offset The zero-based position in  at which to begin sending data.
	 * @param size The number of bytes to send.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP An  that represents the remote device.
	 * @param callback The  delegate.
	 * @param state An object that contains state information for this request.
	 * @return An  that references the asynchronous send.
	 */
	function BeginSendTo(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.system.net.EndPoint, callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Associates a  with a local endpoint.
	 * @param localEP The local  to associate with the .
	 */
	function Bind(localEP:cs.system.net.EndPoint):Void;
	@:overload(function():Void {})
	/** Closes the  connection and releases all associated resources. */
	function Close(timeout:Int):Void;
	@:overload(function(remoteEP:cs.system.net.EndPoint):Void {})
	@:overload(function(address:cs.system.net.IPAddress, port:Int):Void {})
	@:overload(function(addresses:cs.NativeArray<cs.system.net.IPAddress>, port:Int):Void {})
	/**
	 * Establishes a connection to a remote host.
	 * @param remoteEP An  that represents the remote device.
	 */
	function Connect(host:String, port:Int):Void;
	/**
	 * Begins an asynchronous request for a connection to a remote host.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function ConnectAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	/**
	 * Closes the socket connection and allows reuse of the socket.
	 * @param reuseSocket if this socket can be reused after the current connection is
	 * closed; otherwise, .
	 */
	function Disconnect(reuseSocket:Bool):Void;
	/**
	 * Begins an asynchronous request to disconnect from a remote endpoint.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function DisconnectAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Duplicates the socket reference for the target process, and closes the socket
	 * for this process.
	 * @param targetProcessId The ID of the target process where a duplicate of the
	 * socket reference is created.
	 * @return The socket reference to be passed to the target process.
	 */
	function DuplicateAndClose(targetProcessId:Int):cs.system.net.sockets.SocketInformation;
	@:overload(function(asyncResult:cs.system.IAsyncResult):cs.system.net.sockets.Socket {})
	@:overload(function(buffer:cs.Ref<cs.NativeArray<cs.UInt8>>, asyncResult:cs.system.IAsyncResult):cs.system.net.sockets.Socket {})
	/**
	 * Asynchronously accepts an incoming connection attempt and creates a new  object
	 * to handle remote host communication. This method returns a buffer that contains
	 * the initial data transferred.
	 * @param buffer An array of type  that contains the bytes transferred.
	 * @param asyncResult An  object that stores state information for this
	 * asynchronous operation as well as any user defined data.
	 * @return A  object to handle communication with the remote host.
	 */
	function EndAccept(buffer:cs.Ref<cs.NativeArray<cs.UInt8>>, bytesTransferred:cs.Ref<Int>, asyncResult:cs.system.IAsyncResult):cs.system.net.sockets.Socket;
	/**
	 * Ends a pending asynchronous connection request.
	 * @param asyncResult An  that stores state information and any user defined data
	 * for this asynchronous operation.
	 */
	function EndConnect(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Ends a pending asynchronous disconnect request.
	 * @param asyncResult An  object that stores state information and any user-defined
	 * data for this asynchronous operation.
	 */
	function EndDisconnect(asyncResult:cs.system.IAsyncResult):Void;
	@:overload(function(asyncResult:cs.system.IAsyncResult):Int {})
	/**
	 * Ends a pending asynchronous read.
	 * @param asyncResult An  that stores state information and any user defined data
	 * for this asynchronous operation.
	 * @return The number of bytes received.
	 */
	function EndReceive(asyncResult:cs.system.IAsyncResult, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int;
	/**
	 * Ends a pending asynchronous read from a specific endpoint.
	 * @param asyncResult An  that stores state information and any user defined data
	 * for this asynchronous operation.
	 * @param endPoint The source .
	 * @return If successful, the number of bytes received. If unsuccessful, returns 0.
	 */
	function EndReceiveFrom(asyncResult:cs.system.IAsyncResult, endPoint:cs.Ref<cs.system.net.EndPoint>):Int;
	/**
	 * Ends a pending asynchronous read from a specific endpoint. This method also
	 * reveals more information about the packet than .
	 * @param asyncResult An  that stores state information and any user defined data
	 * for this asynchronous operation.
	 * @param socketFlags A bitwise combination of the  values for the received packet.
	 * @param endPoint The source .
	 * @param ipPacketInformation The  and interface of the received packet.
	 * @return If successful, the number of bytes received. If unsuccessful, returns 0.
	 */
	function EndReceiveMessageFrom(asyncResult:cs.system.IAsyncResult, socketFlags:cs.Ref<cs.system.net.sockets.SocketFlags>, endPoint:cs.Ref<cs.system.net.EndPoint>, ipPacketInformation:cs.Ref<cs.system.net.sockets.IPPacketInformation>):Int;
	@:overload(function(asyncResult:cs.system.IAsyncResult):Int {})
	/**
	 * Ends a pending asynchronous send.
	 * @param asyncResult An  that stores state information for this asynchronous
	 * operation.
	 * @return If successful, the number of bytes sent to the ; otherwise, an invalid 
	 * error.
	 */
	function EndSend(asyncResult:cs.system.IAsyncResult, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int;
	/**
	 * Ends a pending asynchronous send of a file.
	 * @param asyncResult An  object that stores state information for this
	 * asynchronous operation.
	 */
	function EndSendFile(asyncResult:cs.system.IAsyncResult):Void;
	/**
	 * Ends a pending asynchronous send to a specific location.
	 * @param asyncResult An  that stores state information and any user defined data
	 * for this asynchronous operation.
	 * @return If successful, the number of bytes sent; otherwise, an invalid  error.
	 */
	function EndSendTo(asyncResult:cs.system.IAsyncResult):Int;
	@:overload(function(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName):Dynamic {})
	@:overload(function(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName, optionValue:cs.NativeArray<cs.UInt8>):Void {})
	/**
	 * Returns the value of a specified  option, represented as an object.
	 * @param optionLevel One of the  values.
	 * @param optionName One of the  values.
	 * @return An object that represents the value of the option. When the  parameter
	 * is set to  the return value is an instance of the  class. When  is set to  or ,
	 * the return value is an instance of the  class. When  is any other value, the
	 * return value is an integer.
	 */
	function GetSocketOption(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName, optionLength:Int):cs.NativeArray<cs.UInt8>;
	@:overload(function(ioControlCode:Int, optionInValue:cs.NativeArray<cs.UInt8>, optionOutValue:cs.NativeArray<cs.UInt8>):Int {})
	/**
	 * Sets low-level operating modes for the  using numerical control codes.
	 * @param ioControlCode An  value that specifies the control code of the operation
	 * to perform.
	 * @param optionInValue A  array that contains the input data required by the
	 * operation.
	 * @param optionOutValue A  array that contains the output data returned by the
	 * operation.
	 * @return The number of bytes in the  parameter.
	 */
	function IOControl(ioControlCode:cs.system.net.sockets.IOControlCode, optionInValue:cs.NativeArray<cs.UInt8>, optionOutValue:cs.NativeArray<cs.UInt8>):Int;
	/**
	 * Places a  in a listening state.
	 * @param backlog The maximum length of the pending connections queue.
	 */
	function Listen(backlog:Int):Void;
	/**
	 * Determines the status of the .
	 * @param microSeconds The time to wait for a response, in microseconds.
	 * @param mode One of the  values.
	 * @return The status of the  based on the polling mode value passed in the 
	 * parameter. Mode Return Value if  has been called and a connection is pending;
	 * -or- if data is available for reading; -or- if the connection has been closed,
	 * reset, or terminated; otherwise, returns . , if processing a , and the
	 * connection has succeeded; -or- if data can be sent; otherwise, returns . if
	 * processing a  that does not block, and the connection has failed; -or- if  is
	 * not set and out-of-band data is available; otherwise, returns .
	 */
	function Poll(microSeconds:Int, mode:cs.system.net.sockets.SelectMode):Bool;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Int {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>):Int {})
	@:overload(function(buffer:cs.system.Span<cs.UInt8>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffer:cs.system.Span<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, size:Int, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int {})
	@:overload(function(buffer:cs.system.Span<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	/**
	 * Receives data from a bound  into a receive buffer.
	 * @param buffer An array of type  that is the storage location for the received
	 * data.
	 * @return The number of bytes received.
	 */
	function Receive(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int;
	/**
	 * Begins an asynchronous request to receive data from a connected  object.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function ReceiveAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, remoteEP:cs.Ref<cs.system.net.EndPoint>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.Ref<cs.system.net.EndPoint>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.Ref<cs.system.net.EndPoint>):Int {})
	/**
	 * Receives the specified number of bytes of data into the specified location of
	 * the data buffer, using the specified , and stores the endpoint.
	 * @param buffer An array of type  that is the storage location for received data.
	 * @param offset The position in the  parameter to store the received data.
	 * @param size The number of bytes to receive.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP An , passed by reference, that represents the remote server.
	 * @return The number of bytes received.
	 */
	function ReceiveFrom(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.Ref<cs.system.net.EndPoint>):Int;
	/**
	 * Begins to asynchronously receive data from a specified network device.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function ReceiveFromAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	/**
	 * Receives the specified number of bytes of data into the specified location of
	 * the data buffer, using the specified , and stores the endpoint and packet
	 * information.
	 * @param buffer An array of type  that is the storage location for received data.
	 * @param offset The position in the  parameter to store the received data.
	 * @param size The number of bytes to receive.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP An , passed by reference, that represents the remote server.
	 * @param ipPacketInformation An  holding address and interface information.
	 * @return The number of bytes received.
	 */
	function ReceiveMessageFrom(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.Ref<cs.system.net.sockets.SocketFlags>, remoteEP:cs.Ref<cs.system.net.EndPoint>, ipPacketInformation:cs.Ref<cs.system.net.sockets.IPPacketInformation>):Int;
	/**
	 * Begins to asynchronously receive the specified number of bytes of data into the
	 * specified location in the data buffer, using the specified , and stores the
	 * endpoint and packet information.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function ReceiveMessageFromAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):Int {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>):Int {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, size:Int, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	@:overload(function(buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int {})
	@:overload(function(buffer:cs.system.ReadOnlySpan<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags):Int {})
	/**
	 * Sends data to a connected .
	 * @param buffer An array of type  that contains the data to be sent.
	 * @return The number of bytes sent to the .
	 */
	function Send(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, errorCode:cs.Ref<cs.system.net.sockets.SocketError>):Int;
	/**
	 * Sends data asynchronously to a connected  object.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function SendAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	@:overload(function(fileName:String):Void {})
	/**
	 * Sends the file  to a connected  object with the  transmit flag.
	 * @param fileName A  that contains the path and name of the file to be sent. This
	 * parameter can be .
	 */
	function SendFile(fileName:String, preBuffer:cs.NativeArray<cs.UInt8>, postBuffer:cs.NativeArray<cs.UInt8>, flags:cs.system.net.sockets.TransmitFileOptions):Void;
	/**
	 * Sends a collection of files or in memory data buffers asynchronously to a
	 * connected  object.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function SendPacketsAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, remoteEP:cs.system.net.EndPoint):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.system.net.EndPoint):Int {})
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.system.net.EndPoint):Int {})
	/**
	 * Sends the specified number of bytes of data to the specified endpoint, starting
	 * at the specified location in the buffer, and using the specified .
	 * @param buffer An array of type  that contains the data to be sent.
	 * @param offset The position in the data buffer at which to begin sending data.
	 * @param size The number of bytes to send.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP The  that represents the destination location for the data.
	 * @return The number of bytes sent.
	 */
	function SendTo(buffer:cs.NativeArray<cs.UInt8>, offset:Int, size:Int, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.system.net.EndPoint):Int;
	/**
	 * Sends data asynchronously to a specific remote host.
	 * @param e The  object to use for this asynchronous socket operation.
	 * @return if the I/O operation is pending. The  event on the  parameter will be
	 * raised upon completion of the operation. if the I/O operation completed
	 * synchronously. In this case, The  event on the  parameter will not be raised and
	 * the  object passed as a parameter may be examined immediately after the method
	 * call returns to retrieve the result of the operation.
	 */
	function SendToAsync(e:cs.system.net.sockets.SocketAsyncEventArgs):Bool;
	/**
	 * Set the IP protection level on a socket.
	 * @param level The IP protection level to set on this socket.
	 */
	function SetIPProtectionLevel(level:cs.system.net.sockets.IPProtectionLevel):Void;
	@:overload(function(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName, optionValue:Bool):Void {})
	@:overload(function(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName, optionValue:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName, optionValue:Int):Void {})
	/**
	 * Sets the specified  option to the specified  value.
	 * @param optionLevel One of the  values.
	 * @param optionName One of the  values.
	 * @param optionValue The value of the option, represented as a .
	 */
	function SetSocketOption(optionLevel:cs.system.net.sockets.SocketOptionLevel, optionName:cs.system.net.sockets.SocketOptionName, optionValue:Dynamic):Void;
	/**
	 * Disables sends and receives on a .
	 * @param how One of the  values that specifies the operation that will no longer
	 * be allowed.
	 */
	function Shutdown(how:cs.system.net.sockets.SocketShutdown):Void;
}
