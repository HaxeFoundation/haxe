package cs.system.net;

/** Manages the collection of  objects. */
@:native("System.Net.ServicePointManager")
extern class ServicePointManager {
	/** The default number of non-persistent connections (4) allowed on a  object connected to an HTTP/1.0 or later server. This field is constant but is no longer used in the .NET Framework 2.0. */
	static var DefaultNonPersistentConnectionLimit(default, never):Int;
	/** The default number of persistent connections (2) allowed on a  object connected to an HTTP/1.1 or later server. This field is constant and is used to initialize the  property if the value of the  property has not been set either directly or through configuration. */
	static var DefaultPersistentConnectionLimit(default, never):Int;
	/**
	 * Gets or sets a  value that indicates whether the certificate is checked against
	 * the certificate authority revocation list.
	 * @return if the certificate revocation list is checked; otherwise, .
	 */
	static var CheckCertificateRevocationList(default, default):Bool;
	/**
	 * Gets or sets the maximum number of concurrent connections allowed by a  object.
	 * @return The maximum number of concurrent connections allowed by a  object. The
	 * default connection limit is 10 for ASP.NET hosted applications and 2 for all
	 * others. When an app is running as an ASP.NET host, it is not possible to alter
	 * the value of this property through the config file if the autoConfig property is
	 * set to . However, you can change the value programmatically when the autoConfig
	 * property is . Set your preferred value once, when the AppDomain loads.
	 */
	static var DefaultConnectionLimit(default, default):Int;
	/**
	 * Gets or sets a value that indicates how long a Domain Name Service (DNS)
	 * resolution is considered valid.
	 * @return The time-out value, in milliseconds. A value of -1 indicates an infinite
	 * time-out period. The default value is 120,000 milliseconds (two minutes).
	 */
	static var DnsRefreshTimeout(default, default):Int;
	/**
	 * Gets or sets a value that indicates whether a Domain Name Service (DNS)
	 * resolution rotates among the applicable Internet Protocol (IP) addresses.
	 * @return if a DNS resolution always returns the first IP address for a particular
	 * host; otherwise . The default is .
	 */
	static var EnableDnsRoundRobin(default, default):Bool;
	/**
	 * Gets the  for this  instance.
	 * @return The encryption policy to use for this  instance.
	 */
	static var EncryptionPolicy(default, never):cs.system.net.security.EncryptionPolicy;
	/**
	 * Gets or sets a  value that determines whether 100-Continue behavior is used.
	 * @return to enable 100-Continue behavior. The default value is .
	 */
	static var Expect100Continue(default, default):Bool;
	/**
	 * Gets or sets the maximum idle time of a  object.
	 * @return The maximum idle time, in milliseconds, of a  object. The default value
	 * is 100,000 milliseconds (100 seconds).
	 */
	static var MaxServicePointIdleTime(default, default):Int;
	/**
	 * Gets or sets the maximum number of  objects to maintain at any time.
	 * @return The maximum number of  objects to maintain. The default value is 0,
	 * which means there is no limit to the number of  objects.
	 */
	static var MaxServicePoints(default, default):Int;
	/**
	 * Setting this property value to  causes all outbound TCP connections from
	 * HttpWebRequest to use the native socket option SO_REUSE_UNICASTPORT on the
	 * socket. This causes the underlying outgoing ports to be shared. This is useful
	 * for scenarios where a large number of outgoing connections are made in a short
	 * time, and the app risks running out of ports.
	 * @return Returns .
	 */
	static var ReusePort(default, default):Bool;
	/**
	 * Gets or sets the security protocol used by the  objects managed by the  object.
	 * @return One of the values defined in the  enumeration.
	 */
	static var SecurityProtocol(default, default):cs.system.net.SecurityProtocolType;
	/**
	 * Gets or sets the callback to validate a server certificate.
	 * @return A . The default value is .
	 */
	static var ServerCertificateValidationCallback(default, default):cs.system.net.security.RemoteCertificateValidationCallback;
	/**
	 * Determines whether the Nagle algorithm is used by the service points managed by
	 * this  object.
	 * @return to use the Nagle algorithm; otherwise, . The default value is .
	 */
	static var UseNagleAlgorithm(default, default):Bool;
	@:overload(function(address:cs.system.Uri):cs.system.net.ServicePoint {})
	@:overload(function(uriString:String, proxy:cs.system.net.IWebProxy):cs.system.net.ServicePoint {})
	/**
	 * Finds an existing  object or creates a new  object to manage communications with
	 * the specified Uniform Resource Identifier (URI).
	 * @param uriString The URI of the Internet resource to be contacted.
	 * @param proxy The proxy data for this request.
	 * @return The  object that manages communications for the request.
	 */
	static function FindServicePoint(address:cs.system.Uri, proxy:cs.system.net.IWebProxy):cs.system.net.ServicePoint;
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
	static function SetTcpKeepAlive(enabled:Bool, keepAliveTime:Int, keepAliveInterval:Int):Void;
}
