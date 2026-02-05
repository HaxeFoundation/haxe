package cs.system.net.sockets;

/** This class contains extension methods to the  class. */
@:native("System.Net.Sockets.SocketTaskExtensions")
extern class SocketTaskExtensions {
	@:overload(function(socket:cs.system.net.sockets.Socket):cs.system.threading.tasks.Task_1<cs.system.net.sockets.Socket> {})
	/**
	 * Performs an asynchronous operation on to accept an incoming connection attempt
	 * on the socket.
	 * @param socket The socket that is listening for connections.
	 * @return An asynchronous task that completes with a  to handle communication with
	 * the remote host.
	 */
	static function AcceptAsync(socket:cs.system.net.sockets.Socket, acceptSocket:cs.system.net.sockets.Socket):cs.system.threading.tasks.Task_1<cs.system.net.sockets.Socket>;
	@:overload(function(socket:cs.system.net.sockets.Socket, remoteEP:cs.system.net.EndPoint):cs.system.threading.tasks.Task {})
	@:overload(function(socket:cs.system.net.sockets.Socket, address:cs.system.net.IPAddress, port:Int):cs.system.threading.tasks.Task {})
	@:overload(function(socket:cs.system.net.sockets.Socket, addresses:cs.NativeArray<cs.system.net.IPAddress>, port:Int):cs.system.threading.tasks.Task {})
	/**
	 * Establishes a connection to a remote host.
	 * @param socket The socket that is used for establishing a connection.
	 * @param remoteEP An EndPoint that represents the remote device.
	 * @return An asynchronous Task.
	 */
	static function ConnectAsync(socket:cs.system.net.sockets.Socket, host:String, port:Int):cs.system.threading.tasks.Task;
	@:overload(function(socket:cs.system.net.sockets.Socket, buffer:cs.system.ArraySegment<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags):cs.system.threading.tasks.Task_1<Int> {})
	@:overload(function(socket:cs.system.net.sockets.Socket, buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags):cs.system.threading.tasks.Task_1<Int> {})
	/**
	 * Receives data from a connected socket.
	 * @param socket The socket to perform the receive operation on.
	 * @param buffer An array that is the storage location for the received data.
	 * @param socketFlags A bitwise combination of the  values.
	 * @return A task that represents the asynchronous receive operation. The value of
	 * the  parameter contains the number of bytes received.
	 */
	static function ReceiveAsync(socket:cs.system.net.sockets.Socket, buffer:cs.system.Memory<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int>;
	/**
	 * Receives data from a specified network device.
	 * @param socket The socket to perform the ReceiveFrom operation on.
	 * @param buffer An array of type Byte that is the storage location for the
	 * received data.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEndPoint An EndPoint that represents the source of the data.
	 * @return An asynchronous Task that completes with a SocketReceiveFromResult
	 * struct.
	 */
	static function ReceiveFromAsync(socket:cs.system.net.sockets.Socket, buffer:cs.system.ArraySegment<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, remoteEndPoint:cs.system.net.EndPoint):cs.system.threading.tasks.Task_1<cs.system.net.sockets.SocketReceiveFromResult>;
	/**
	 * Receives the specified number of bytes of data into the specified location of
	 * the data buffer, using the specified , and stores the endpoint and packet
	 * information.
	 * @param socket The socket to perform the operation on.
	 * @param buffer An array that is the storage location for received data.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEndPoint An , that represents the remote server.
	 * @return An asynchronous Task that completes with a  struct.
	 */
	static function ReceiveMessageFromAsync(socket:cs.system.net.sockets.Socket, buffer:cs.system.ArraySegment<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, remoteEndPoint:cs.system.net.EndPoint):cs.system.threading.tasks.Task_1<cs.system.net.sockets.SocketReceiveMessageFromResult>;
	@:overload(function(socket:cs.system.net.sockets.Socket, buffer:cs.system.ArraySegment<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags):cs.system.threading.tasks.Task_1<Int> {})
	@:overload(function(socket:cs.system.net.sockets.Socket, buffers:cs.system.collections.generic.IList<cs.system.ArraySegment<cs.UInt8>>, socketFlags:cs.system.net.sockets.SocketFlags):cs.system.threading.tasks.Task_1<Int> {})
	/**
	 * Sends data to a connected socket.
	 * @param socket The socket to perform the operation on.
	 * @param buffer An array of type Byte that contains the data to send.
	 * @param socketFlags A bitwise combination of the  values.
	 * @return An asynchronous task that completes with number of bytes sent to the
	 * socket if the operation was successful. Otherwise, the task will complete with
	 * an invalid socket error.
	 */
	static function SendAsync(socket:cs.system.net.sockets.Socket, buffer:cs.system.ReadOnlyMemory<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, ?cancellationToken:cs.system.threading.CancellationToken):cs.system.threading.tasks.ValueTask_1<Int>;
	/**
	 * Sends data asynchronously to a specific remote host.
	 * @param socket The socket to perform the operation on.
	 * @param buffer An array that contains the data to send.
	 * @param socketFlags A bitwise combination of the  values.
	 * @param remoteEP An  that represents the remote device.
	 * @return An asynchronous task that completes with number of bytes sent if the
	 * operation was successful. Otherwise, the task will complete with an invalid
	 * socket error.
	 */
	static function SendToAsync(socket:cs.system.net.sockets.Socket, buffer:cs.system.ArraySegment<cs.UInt8>, socketFlags:cs.system.net.sockets.SocketFlags, remoteEP:cs.system.net.EndPoint):cs.system.threading.tasks.Task_1<Int>;
}
