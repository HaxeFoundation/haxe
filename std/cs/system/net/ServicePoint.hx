package cs.system.net;

/** Provides connection management for HTTP connections. */
@:native("System.Net.ServicePoint")
extern class ServicePoint {
	/**
	 * Gets the Uniform Resource Identifier (URI) of the server that this  object
	 * connects to.
	 * @return An instance of the  class that contains the URI of the Internet server
	 * that this  object connects to.
	 */
	var Address(default, never):cs.system.Uri;
	/**
	 * Specifies the delegate to associate a local  with a .
	 * @return A delegate that forces a  to use a particular local Internet Protocol
	 * (IP) address and port number. The default value is .
	 */
	var BindIPEndPointDelegate(default, default):cs.system.net.BindIPEndPoint;
	/**
	 * Gets the certificate received for this  object.
	 * @return An instance of the  class that contains the security certificate
	 * received for this  object.
	 */
	var Certificate(default, never):cs.system.security.cryptography.x509certificates.X509Certificate;
	/**
	 * Gets the last client certificate sent to the server.
	 * @return An  object that contains the public values of the last client
	 * certificate sent to the server.
	 */
	var ClientCertificate(default, never):cs.system.security.cryptography.x509certificates.X509Certificate;
	/**
	 * Gets or sets the number of milliseconds after which an active  connection is
	 * closed.
	 * @return A  that specifies the number of milliseconds that an active  connection
	 * remains open. The default is -1, which allows an active  connection to stay
	 * connected indefinitely. Set this property to 0 to force  connections to close
	 * after servicing a request.
	 */
	var ConnectionLeaseTimeout(default, default):Int;
	/**
	 * Gets or sets the maximum number of connections allowed on this  object.
	 * @return The maximum number of connections allowed on this  object.
	 */
	var ConnectionLimit(default, default):Int;
	/**
	 * Gets the connection name.
	 * @return A  that represents the connection name.
	 */
	var ConnectionName(default, never):String;
	/**
	 * Gets the number of open connections associated with this  object.
	 * @return The number of open connections associated with this  object.
	 */
	var CurrentConnections(default, never):Int;
	/**
	 * Gets or sets a  value that determines whether 100-Continue behavior is used.
	 * @return to expect 100-Continue responses for  requests; otherwise, . The default
	 * value is .
	 */
	var Expect100Continue(default, default):Bool;
	/**
	 * Gets the date and time that the  object was last connected to a host.
	 * @return A  object that contains the date and time at which the  object was last
	 * connected.
	 */
	var IdleSince(default, never):cs.system.DateTime;
	/**
	 * Gets or sets the amount of time a connection associated with the  object can
	 * remain idle before the connection is closed.
	 * @return The length of time, in milliseconds, that a connection associated with
	 * the  object can remain idle before it is closed and reused for another
	 * connection.
	 */
	var MaxIdleTime(default, default):Int;
	/**
	 * Gets the version of the HTTP protocol that the  object uses.
	 * @return A  object that contains the HTTP protocol version that the  object uses.
	 */
	var ProtocolVersion(default, never):cs.system.Version;
	/**
	 * Gets or sets the size of the receiving buffer for the socket used by this .
	 * @return A  that contains the size, in bytes, of the receive buffer. The default
	 * is 8192.
	 */
	var ReceiveBufferSize(default, default):Int;
	/**
	 * Indicates whether the  object supports pipelined connections.
	 * @return if the  object supports pipelined connections; otherwise, .
	 */
	var SupportsPipelining(default, never):Bool;
	/**
	 * Gets or sets a  value that determines whether the Nagle algorithm is used on
	 * connections managed by this  object.
	 * @return to use the Nagle algorithm; otherwise, . The default value is .
	 */
	var UseNagleAlgorithm(default, default):Bool;
	/**
	 * Removes the specified connection group from this  object.
	 * @param connectionGroupName The name of the connection group that contains the
	 * connections to close and remove from this service point.
	 * @return A  value that indicates whether the connection group was closed.
	 */
	function CloseConnectionGroup(connectionGroupName:String):Bool;
	/**
	 * Enables or disables the keep-alive option on a TCP connection.
	 * @param enabled If set to true, then the TCP keep-alive option on a TCP
	 * connection will be enabled using the specified  and  values. If set to false,
	 * then the TCP keep-alive option is disabled and the remaining parameters are
	 * ignored. The default value is false.
	 * @param keepAliveTime Specifies the timeout, in milliseconds, with no activity
	 * until the first keep-alive packet is sent. The value must be greater than 0.  If
	 * a value of less than or equal to zero is passed an  is thrown.
	 * @param keepAliveInterval Specifies the interval, in milliseconds, between when
	 * successive keep-alive packets are sent if no acknowledgement is received. The
	 * value must be greater than 0.  If a value of less than or equal to zero is
	 * passed an  is thrown.
	 */
	function SetTcpKeepAlive(enabled:Bool, keepAliveTime:Int, keepAliveInterval:Int):Void;
}
