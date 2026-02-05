package cs.system.net.http;

/** A helper class for retrieving and comparing standard HTTP methods and for creating new HTTP methods. */
@:native("System.Net.Http.HttpMethod")
extern class HttpMethod {
	/**
	 * Represents an HTTP DELETE protocol method.
	 * @return Returns .
	 */
	static var Delete(default, never):cs.system.net.http.HttpMethod;
	/**
	 * Represents an HTTP GET protocol method.
	 * @return Returns .
	 */
	static var Get(default, never):cs.system.net.http.HttpMethod;
	/**
	 * Represents an HTTP HEAD protocol method. The HEAD method is identical to GET
	 * except that the server only returns message-headers in the response, without a
	 * message-body.
	 * @return Returns .
	 */
	static var Head(default, never):cs.system.net.http.HttpMethod;
	/**
	 * Represents an HTTP OPTIONS protocol method.
	 * @return Returns .
	 */
	static var Options(default, never):cs.system.net.http.HttpMethod;
	static var Patch(default, never):cs.system.net.http.HttpMethod;
	/**
	 * Represents an HTTP POST protocol method that is used to post a new entity as an
	 * addition to a URI.
	 * @return Returns .
	 */
	static var Post(default, never):cs.system.net.http.HttpMethod;
	/**
	 * Represents an HTTP PUT protocol method that is used to replace an entity
	 * identified by a URI.
	 * @return Returns .
	 */
	static var Put(default, never):cs.system.net.http.HttpMethod;
	/**
	 * Represents an HTTP TRACE protocol method.
	 * @return Returns .
	 */
	static var Trace(default, never):cs.system.net.http.HttpMethod;
	/**
	 * An HTTP method.
	 * @return An HTTP method represented as a .
	 */
	var Method(default, never):String;
	function new(method:String):Void;
	/**
	 * The equality operator for comparing two  objects.
	 * @param left The left  to an equality operator.
	 * @param right The right   to an equality operator.
	 * @return if the specified  and  parameters are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.net.http.HttpMethod, right:cs.system.net.http.HttpMethod):Bool;
	/**
	 * The inequality operator for comparing two  objects.
	 * @param left The left  to an inequality operator.
	 * @param right The right   to an inequality operator.
	 * @return if the specified  and  parameters are inequal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.net.http.HttpMethod, right:cs.system.net.http.HttpMethod):Bool;
	@:overload(function(other:cs.system.net.http.HttpMethod):Bool {})
	/**
	 * Determines whether the specified  is equal to the current .
	 * @param other The HTTP method to compare with the current object.
	 * @return if the specified object is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for this type.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current object.
	 * @return A string representing the current object.
	 */
	function ToString():String;
}
