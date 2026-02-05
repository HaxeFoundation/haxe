package cs.system.net;

/** Provides a set of properties and methods that are used to manage cookies. This class cannot be inherited. */
@:native("System.Net.Cookie")
extern class Cookie {
	/**
	 * Gets or sets a comment that the server can add to a .
	 * @return An optional comment to document intended usage for this .
	 */
	var Comment(default, default):String;
	/**
	 * Gets or sets a URI comment that the server can provide with a .
	 * @return An optional comment that represents the intended usage of the URI
	 * reference for this . The value must conform to URI format.
	 */
	var CommentUri(default, default):cs.system.Uri;
	/**
	 * Gets or sets the discard flag set by the server.
	 * @return if the client is to discard the  at the end of the current session;
	 * otherwise, . The default is .
	 */
	var Discard(default, default):Bool;
	/**
	 * Gets or sets the URI for which the  is valid.
	 * @return The URI for which the  is valid.
	 */
	var Domain(default, default):String;
	/**
	 * Gets or sets the current state of the .
	 * @return if the  has expired; otherwise, . The default is .
	 */
	var Expired(default, default):Bool;
	/**
	 * Gets or sets the expiration date and time for the  as a .
	 * @return The expiration date and time for the  as a  instance.
	 */
	var Expires(default, default):cs.system.DateTime;
	/**
	 * Determines whether a page script or other active content can access this cookie.
	 * @return Boolean value that determines whether a page script or other active
	 * content can access this cookie.
	 */
	var HttpOnly(default, default):Bool;
	/**
	 * Gets or sets the name for the .
	 * @return The name for the .
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the URIs to which the  applies.
	 * @return The URIs to which the  applies.
	 */
	var Path(default, default):String;
	/**
	 * Gets or sets a list of TCP ports that the  applies to.
	 * @return The list of TCP ports that the  applies to.
	 */
	var Port(default, default):String;
	/**
	 * Gets or sets the security level of a .
	 * @return if the client is only to return the cookie in subsequent requests if
	 * those requests use Secure Hypertext Transfer Protocol (HTTPS); otherwise, . The
	 * default is .
	 */
	var Secure(default, default):Bool;
	/**
	 * Gets the time when the cookie was issued as a .
	 * @return The time when the cookie was issued as a .
	 */
	var TimeStamp(default, never):cs.system.DateTime;
	/**
	 * Gets or sets the  for the .
	 * @return The  for the .
	 */
	var Value(default, default):String;
	/**
	 * Gets or sets the version of HTTP state maintenance to which the cookie conforms.
	 * @return The version of HTTP state maintenance to which the cookie conforms.
	 */
	var Version(default, default):Int;
	@:overload(function():Void {})
	@:overload(function(name:String, value:String):Void {})
	@:overload(function(name:String, value:String, path:String):Void {})
	function new(name:String, value:String, path:String, domain:String):Void;
	/**
	 * Overrides the  method.
	 * @param comparand A reference to a .
	 * @return Returns  if the  is equal to . Two  instances are equal if their , , , ,
	 * and  properties are equal.  and  string comparisons are case-insensitive.
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Overrides the  method.
	 * @return The 32-bit signed integer hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Overrides the  method.
	 * @return Returns a string representation of this  object that is suitable for
	 * including in a HTTP Cookie: request header.
	 */
	function ToString():String;
}
