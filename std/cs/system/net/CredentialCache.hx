package cs.system.net;

/** Provides storage for multiple credentials. */
@:native("System.Net.CredentialCache")
extern class CredentialCache {
	/**
	 * Gets the system credentials of the application.
	 * @return An  that represents the system credentials of the application.
	 */
	static var DefaultCredentials(default, never):cs.system.net.ICredentials;
	/**
	 * Gets the network credentials of the current security context.
	 * @return An  that represents the network credentials of the current user or
	 * application.
	 */
	static var DefaultNetworkCredentials(default, never):cs.system.net.NetworkCredential;
	function new():Void;
	@:overload(function(uriPrefix:cs.system.Uri, authType:String, cred:cs.system.net.NetworkCredential):Void {})
	/**
	 * Adds a  instance for use with SMTP to the credential cache and associates it
	 * with a host computer, port, and authentication protocol. Credentials added using
	 * this method are valid for SMTP only. This method does not work for HTTP or FTP
	 * requests.
	 * @param host A  that identifies the host computer.
	 * @param port A  that specifies the port to connect to on .
	 * @param authenticationType A  that identifies the authentication scheme used when
	 * connecting to  using .
	 * @param credential The  to add to the credential cache.
	 */
	function Add(host:String, port:Int, authenticationType:String, credential:cs.system.net.NetworkCredential):Void;
	@:overload(function(uriPrefix:cs.system.Uri, authType:String):cs.system.net.NetworkCredential {})
	/**
	 * Returns the  instance associated with the specified host, port, and
	 * authentication protocol.
	 * @param host A  that identifies the host computer.
	 * @param port A  that specifies the port to connect to on .
	 * @param authenticationType A  that identifies the authentication scheme used when
	 * connecting to .
	 * @return A  or, if there is no matching credential in the cache, .
	 */
	function GetCredential(host:String, port:Int, authenticationType:String):cs.system.net.NetworkCredential;
	/**
	 * Returns an enumerator that can iterate through the  instance.
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	@:overload(function(uriPrefix:cs.system.Uri, authType:String):Void {})
	/**
	 * Deletes a  instance from the cache if it is associated with the specified host,
	 * port, and authentication protocol.
	 * @param host A  that identifies the host computer.
	 * @param port A  that specifies the port to connect to on .
	 * @param authenticationType A  that identifies the authentication scheme used when
	 * connecting to .
	 */
	function Remove(host:String, port:Int, authenticationType:String):Void;
}
