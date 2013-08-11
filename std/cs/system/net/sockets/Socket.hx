
package cs.system.net.sockets;

@:native("System.Net.Sockets.Socket") 
extern class Socket {
	public function new ( addressFamily : AddressFamily, socketType : SocketType, protocolType : ProtocolType );
	
	/**
	 * Gets the amount of data that has been received from the network and is available to be read.
	 */
	public var Available (default, never) : Int;
	
	/**
	 * Gets or sets a value that indicates whether the Socket is in blocking mode.
	 */
	public var Blocking (default, default) : Bool;
	
	/**
	 * Gets a value that indicates whether a Socket is connected to a remote host as of the last Send or Receive operation.
	 */
	public var Connected (default, never) : Bool;
	
	/**
	 * The LocalEndPoint property gets an EndPoint that contains the local IP address and port number to which your Socket is bound.
	 * You must cast this EndPoint to an IPEndPoint before retrieving any information.
	 */
	public var LocalEndPoint (default, never) : cs.system.net.EndPoint;
	
	/**
	 * Gets or sets a Boolean value that specifies whether the stream Socket is using the Nagle algorithm.
	 */
	public var NoDelay(default, default) : Bool;
	
	/**
	 * The time-out value, in milliseconds. 
	 * The default value is 0, which indicates an infinite time-out period. Specifying -1 also indicates an infinite time-out period.
	 */
	public var ReceiveTimeout (default, default) : Int;
	
	/**
	 * If you are using a connection-oriented protocol, the RemoteEndPoint property gets the EndPoint that contains the remote IP address and port number to which the Socket is connected.
	 */
	public var RemoteEndPoint (default, null) : cs.system.net.EndPoint;
	
	/**
	 * The time-out value, in milliseconds. 
	 * If you set the property with a value between 1 and 499, the value will be changed to 500. The default value is 0, which indicates an infinite time-out period. Specifying -1 also indicates an infinite time-out period.
	 */
	public var SendTimeout (default, default) : Int;
	
	
	/**
	 * Creates a new Socket for a newly created connection.
	 */
	public function Accept() : Socket;
	
	/**
	 * Associates a Socket with a local endpoint.
	 */
	public function Bind(localEP : EndPoint) : Void;
	
	/**
	 * Establishes a connection to a remote host. The host is specified by an IP address and a port number.
	 */
	public function Connect(address : IPAddress, port : Int) : Void;
	
	/**
	 * Closes the Socket connection and releases all associated resources.
	 */
	public function Close() : Void;
	
	/**
	 * Places a Socket in a listening state.
	 * @param	backlog		The maximum length of the pending connections queue. 
	 */
	public function Listen(	backlog : Int ) : Void;
	
	/**
	 * Receives the specified number of bytes of data from a bound Socket into a receive buffer, using the specified SocketFlags.
	 * @return The number of bytes received.
	 */
	@:overload(function ( buffer : haxe.io.BytesData ) : Int { } ) 
	public function Receive( buffer : haxe.io.BytesData, size : Int, socketFlags : SocketFlags ) : Int;
	
	/**
	 * Send synchronously sends data to the remote host specified in the Connect or Accept method and returns the number of bytes successfully sent.
	 */
	public function Send( buffer : haxe.io.BytesData ) : Int;
	
	/**
	 * Disables sends and receives on a Socket.
	 * @param	how		One of the SocketShutdown values that specifies the operation that will no longer be allowed.
	 */
	public function Shutdown(how : SocketShutdown) : Void;
}
