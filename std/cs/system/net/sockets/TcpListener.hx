package cs.system.net.sockets;

/** Listens for connections from TCP network clients. */
@:native("System.Net.Sockets.TcpListener")
extern class TcpListener {
	/**
	 * Gets a value that indicates whether  is actively listening for client
	 * connections.
	 * @return if  is actively listening; otherwise, .
	 */
	var Active(default, never):Bool;
	/**
	 * Gets or sets a  value that specifies whether the  allows only one underlying
	 * socket to listen to a specific port.
	 * @return if the  allows only one  to listen to a specific port; otherwise, . .
	 * The default is  for Windows Server 2003 and Windows XP Service Pack 2 and later,
	 * and  for all other versions.
	 */
	var ExclusiveAddressUse(default, default):Bool;
	/**
	 * Gets the underlying  of the current .
	 * @return The  to which the  is bound.
	 */
	var LocalEndpoint(default, never):cs.system.net.EndPoint;
	/**
	 * Gets the underlying network .
	 * @return The underlying .
	 */
	var Server(default, never):cs.system.net.sockets.Socket;
	@:overload(function(port:Int):Void {})
	@:overload(function(localEP:cs.system.net.IPEndPoint):Void {})
	function new(localaddr:cs.system.net.IPAddress, port:Int):Void;
	/**
	 * Creates a new  instance to listen on the specified port.
	 * @param port The port on which to listen for incoming connection attempts.
	 * @return A new  instance to listen on the specified port.
	 */
	static function Create(port:Int):cs.system.net.sockets.TcpListener;
	/**
	 * Accepts a pending connection request.
	 * @return A  used to send and receive data.
	 */
	function AcceptSocket():cs.system.net.sockets.Socket;
	/**
	 * Accepts a pending connection request as an asynchronous operation.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  used to send and receive data.
	 */
	function AcceptSocketAsync():cs.system.threading.tasks.Task_1<cs.system.net.sockets.Socket>;
	/**
	 * Accepts a pending connection request.
	 * @return A  used to send and receive data.
	 */
	function AcceptTcpClient():cs.system.net.sockets.TcpClient;
	/**
	 * Accepts a pending connection request as an asynchronous operation.
	 * @return The task object representing the asynchronous operation. The  property
	 * on the task object returns a  used to send and receive data.
	 */
	function AcceptTcpClientAsync():cs.system.threading.tasks.Task_1<cs.system.net.sockets.TcpClient>;
	/**
	 * Enables or disables Network Address Translation (NAT) traversal on a  instance.
	 * @param allowed A Boolean value that specifies whether to enable or disable NAT
	 * traversal.
	 */
	function AllowNatTraversal(allowed:Bool):Void;
	/**
	 * Begins an asynchronous operation to accept an incoming connection attempt.
	 * @param callback An  delegate that references the method to invoke when the
	 * operation is complete.
	 * @param state A user-defined object containing information about the accept
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  that references the asynchronous creation of the .
	 */
	function BeginAcceptSocket(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Begins an asynchronous operation to accept an incoming connection attempt.
	 * @param callback An  delegate that references the method to invoke when the
	 * operation is complete.
	 * @param state A user-defined object containing information about the accept
	 * operation. This object is passed to the  delegate when the operation is
	 * complete.
	 * @return An  that references the asynchronous creation of the .
	 */
	function BeginAcceptTcpClient(callback:cs.system.AsyncCallback, state:Dynamic):cs.system.IAsyncResult;
	/**
	 * Asynchronously accepts an incoming connection attempt and creates a new  to
	 * handle remote host communication.
	 * @param asyncResult An  returned by a call to the  method.
	 * @return A . The  used to send and receive data.
	 */
	function EndAcceptSocket(asyncResult:cs.system.IAsyncResult):cs.system.net.sockets.Socket;
	/**
	 * Asynchronously accepts an incoming connection attempt and creates a new  to
	 * handle remote host communication.
	 * @param asyncResult An  returned by a call to the  method.
	 * @return A . The  used to send and receive data.
	 */
	function EndAcceptTcpClient(asyncResult:cs.system.IAsyncResult):cs.system.net.sockets.TcpClient;
	/**
	 * Determines if there are pending connection requests.
	 * @return if connections are pending; otherwise, .
	 */
	function Pending():Bool;
	@:overload(function():Void {})
	/** Starts listening for incoming connection requests. */
	function Start(backlog:Int):Void;
	/** Closes the listener. */
	function Stop():Void;
}
