package cs.system.net;

/** Contains HTTP proxy settings for the  class. */
@:native("System.Net.WebProxy")
extern class WebProxy {
	/**
	 * Gets or sets the address of the proxy server.
	 * @return A  instance that contains the address of the proxy server.
	 */
	var Address(default, default):cs.system.Uri;
	/**
	 * Gets a list of addresses that do not use the proxy server.
	 * @return An  that contains a list of  arrays that represents URIs that do not use
	 * the proxy server when accessed.
	 */
	var BypassArrayList(default, never):cs.system.collections.ArrayList;
	/**
	 * Gets or sets an array of addresses that do not use the proxy server.
	 * @return An array that contains a list of regular expressions that describe URIs
	 * that do not use the proxy server when accessed.
	 */
	var BypassList(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets a value that indicates whether to bypass the proxy server for local
	 * addresses.
	 * @return to bypass the proxy server for local addresses; otherwise, . The default
	 * value is .
	 */
	var BypassProxyOnLocal(default, default):Bool;
	/**
	 * Gets or sets the credentials to submit to the proxy server for authentication.
	 * @return An  instance that contains the credentials to submit to the proxy server
	 * for authentication.
	 */
	var Credentials(default, default):cs.system.net.ICredentials;
	/**
	 * Gets or sets a  value that controls whether the  are sent with requests.
	 * @return if the default credentials are used; otherwise, . The default value is .
	 */
	var UseDefaultCredentials(default, default):Bool;
	@:overload(function():Void {})
	@:overload(function(Address:String):Void {})
	@:overload(function(Address:cs.system.Uri):Void {})
	@:overload(function(Address:String, BypassOnLocal:Bool):Void {})
	@:overload(function(Host:String, Port:Int):Void {})
	@:overload(function(Address:cs.system.Uri, BypassOnLocal:Bool):Void {})
	@:overload(function(Address:String, BypassOnLocal:Bool, BypassList:cs.NativeArray<String>):Void {})
	@:overload(function(Address:cs.system.Uri, BypassOnLocal:Bool, BypassList:cs.NativeArray<String>):Void {})
	@:overload(function(Address:String, BypassOnLocal:Bool, BypassList:cs.NativeArray<String>, Credentials:cs.system.net.ICredentials):Void {})
	function new(Address:cs.system.Uri, BypassOnLocal:Bool, BypassList:cs.NativeArray<String>, Credentials:cs.system.net.ICredentials):Void;
	/**
	 * Reads the Internet Explorer nondynamic proxy settings.
	 * @return A  instance that contains the nondynamic proxy settings from Internet
	 * Explorer 5.5 and later.
	 */
	static function GetDefaultProxy():cs.system.net.WebProxy;
	/**
	 * Returns the proxied URI for a request.
	 * @param destination The  instance of the requested Internet resource.
	 * @return The  instance of the Internet resource, if the resource is on the bypass
	 * list; otherwise, the  instance of the proxy.
	 */
	function GetProxy(destination:cs.system.Uri):cs.system.Uri;
	/**
	 * Indicates whether to use the proxy server for the specified host.
	 * @param host The  instance of the host to check for proxy use.
	 * @return if the proxy server should not be used for ; otherwise, .
	 */
	function IsBypassed(host:cs.system.Uri):Bool;
}
