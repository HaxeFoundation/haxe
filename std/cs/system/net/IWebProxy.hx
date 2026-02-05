package cs.system.net;

/** Provides the base interface for implementation of proxy access for the  class. */
@:native("System.Net.IWebProxy")
extern interface IWebProxy {
	/**
	 * The credentials to submit to the proxy server for authentication.
	 * @return An  instance that contains the credentials that are needed to
	 * authenticate a request to the proxy server.
	 */
	var Credentials(default, default):cs.system.net.ICredentials;
	/**
	 * Returns the URI of a proxy.
	 * @param destination A  that specifies the requested Internet resource.
	 * @return A  instance that contains the URI of the proxy used to contact .
	 */
	function GetProxy(destination:cs.system.Uri):cs.system.Uri;
	/**
	 * Indicates that the proxy should not be used for the specified host.
	 * @param host The  of the host to check for proxy use.
	 * @return if the proxy server should not be used for ; otherwise, .
	 */
	function IsBypassed(host:cs.system.Uri):Bool;
}
