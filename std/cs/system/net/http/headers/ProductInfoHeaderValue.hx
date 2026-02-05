package cs.system.net.http.headers;

/** Represents a value which can either be a product or a comment in a User-Agent header. */
@:native("System.Net.Http.Headers.ProductInfoHeaderValue")
extern class ProductInfoHeaderValue {
	/**
	 * Gets the comment from the  object.
	 * @return The comment value this .
	 */
	var Comment(default, never):String;
	/**
	 * Gets the product from the  object.
	 * @return The product value from this .
	 */
	var Product(default, never):cs.system.net.http.headers.ProductHeaderValue;
	@:overload(function(product:cs.system.net.http.headers.ProductHeaderValue):Void {})
	@:overload(function(comment:String):Void {})
	function new(productName:String, productVersion:String):Void;
	/**
	 * Converts a string to an  instance.
	 * @param input A string that represents product info header value information.
	 * @return A  instance.
	 */
	static function Parse(input:String):cs.system.net.http.headers.ProductInfoHeaderValue;
	/**
	 * Determines whether a string is valid  information.
	 * @param input The string to validate.
	 * @param parsedValue The  version of the string.
	 * @return if  is valid  information; otherwise, .
	 */
	static function TryParse(input:String, parsedValue:cs.Ref<cs.system.net.http.headers.ProductInfoHeaderValue>):Bool;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param obj The object to compare with the current object.
	 * @return if the specified  is equal to the current object; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for an  object.
	 * @return A hash code for the current object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
}
