package cs.system.net;

/** Provides a container for a collection of  objects. */
@:native("System.Net.CookieContainer")
extern class CookieContainer {
	/** Represents the default maximum size, in bytes, of the  instances that the  can hold. This field is constant. */
	static var DefaultCookieLengthLimit(default, never):Int;
	/** Represents the default maximum number of  instances that the  can hold. This field is constant. */
	static var DefaultCookieLimit(default, never):Int;
	/** Represents the default maximum number of  instances that the  can reference per domain. This field is constant. */
	static var DefaultPerDomainCookieLimit(default, never):Int;
	/**
	 * Gets or sets the number of  instances that a  can hold.
	 * @return The number of  instances that a  can hold. This is a hard limit and
	 * cannot be exceeded by adding a .
	 */
	var Capacity(default, default):Int;
	/**
	 * Gets the number of  instances that a  currently holds.
	 * @return The number of  instances that a  currently holds. This is the total of 
	 * instances in all domains.
	 */
	var Count(default, never):Int;
	/**
	 * Represents the maximum allowed length of a .
	 * @return The maximum allowed length, in bytes, of a .
	 */
	var MaxCookieSize(default, default):Int;
	/**
	 * Gets or sets the number of  instances that a  can hold per domain.
	 * @return The number of  instances that are allowed per domain.
	 */
	var PerDomainCapacity(default, default):Int;
	@:overload(function():Void {})
	@:overload(function(capacity:Int):Void {})
	function new(capacity:Int, perDomainCapacity:Int, maxCookieSize:Int):Void;
	@:overload(function(cookie:cs.system.net.Cookie):Void {})
	@:overload(function(cookies:cs.system.net.CookieCollection):Void {})
	@:overload(function(uri:cs.system.Uri, cookie:cs.system.net.Cookie):Void {})
	/**
	 * Adds a  to a . This method uses the domain from the  to determine which domain
	 * collection to associate the  with.
	 * @param cookie The  to be added to the .
	 */
	function Add(uri:cs.system.Uri, cookies:cs.system.net.CookieCollection):Void;
	/**
	 * Gets the HTTP cookie header that contains the HTTP cookies that represent the 
	 * instances that are associated with a specific URI.
	 * @param uri The URI of the  instances desired.
	 * @return An HTTP cookie header, with strings representing  instances delimited by
	 * semicolons.
	 */
	function GetCookieHeader(uri:cs.system.Uri):String;
	/**
	 * Gets a  that contains the  instances that are associated with a specific URI.
	 * @param uri The URI of the  instances desired.
	 * @return A  that contains the  instances that are associated with a specific URI.
	 */
	function GetCookies(uri:cs.system.Uri):cs.system.net.CookieCollection;
	/**
	 * Adds  instances for one or more cookies from an HTTP cookie header to the  for a
	 * specific URI.
	 * @param uri The URI of the .
	 * @param cookieHeader The contents of an HTTP set-cookie header as returned by a
	 * HTTP server, with  instances delimited by commas.
	 */
	function SetCookies(uri:cs.system.Uri, cookieHeader:String):Void;
}
