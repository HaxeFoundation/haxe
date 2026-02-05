package cs.system;

/** Provides a custom constructor for uniform resource identifiers (URIs) and modifies URIs for the  class. */
@:native("System.UriBuilder")
extern class UriBuilder {
	/**
	 * Gets or sets the fragment portion of the URI.
	 * @return The fragment portion of the URI. The fragment identifier ("#") is added
	 * to the beginning of the fragment.
	 */
	var Fragment(default, default):String;
	/**
	 * Gets or sets the Domain Name System (DNS) host name or IP address of a server.
	 * @return The DNS host name or IP address of the server.
	 */
	var Host(default, default):String;
	/**
	 * Gets or sets the password associated with the user that accesses the URI.
	 * @return The password of the user that accesses the URI.
	 */
	var Password(default, default):String;
	/**
	 * Gets or sets the path to the resource referenced by the URI.
	 * @return The path to the resource referenced by the URI.
	 */
	var Path(default, default):String;
	/**
	 * Gets or sets the port number of the URI.
	 * @return The port number of the URI.
	 */
	var Port(default, default):Int;
	/**
	 * Gets or sets any query information included in the URI.
	 * @return The query information included in the URI.
	 */
	var Query(default, default):String;
	/**
	 * Gets or sets the scheme name of the URI.
	 * @return The scheme of the URI.
	 */
	var Scheme(default, default):String;
	/**
	 * Gets the  instance constructed by the specified  instance.
	 * @return A  that contains the URI constructed by the .
	 */
	var Uri(default, never):cs.system.Uri;
	/**
	 * The user name associated with the user that accesses the URI.
	 * @return The user name of the user that accesses the URI.
	 */
	var UserName(default, default):String;
	@:overload(function():Void {})
	@:overload(function(uri:String):Void {})
	@:overload(function(uri:cs.system.Uri):Void {})
	@:overload(function(schemeName:String, hostName:String):Void {})
	@:overload(function(scheme:String, host:String, portNumber:Int):Void {})
	@:overload(function(scheme:String, host:String, port:Int, pathValue:String):Void {})
	function new(scheme:String, host:String, port:Int, path:String, extraValue:String):Void;
	/**
	 * Compares an existing  instance with the contents of the  for equality.
	 * @param rparam The object to compare with the current instance.
	 * @return if  represents the same  as the  constructed by this  instance;
	 * otherwise, .
	 */
	function Equals(rparam:Dynamic):Bool;
	/**
	 * Returns the hash code for the URI.
	 * @return The hash code generated for the URI.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the display string for the specified  instance.
	 * @return The string that contains the unescaped display string of the .
	 */
	function ToString():String;
}
