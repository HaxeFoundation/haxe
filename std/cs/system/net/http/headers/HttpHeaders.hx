package cs.system.net.http.headers;

/** A collection of headers and their values as defined in RFC 2616. */
@:native("System.Net.Http.Headers.HttpHeaders")
extern class HttpHeaders {
	@:overload(function(name:String, values:cs.system.collections.generic.IEnumerable<String>):Void {})
	/**
	 * Adds the specified header and its values into the  collection.
	 * @param name The header to add to the collection.
	 * @param values A list of header values to add to the collection.
	 */
	function Add(name:String, value:String):Void;
	/** Removes all headers from the  collection. */
	function Clear():Void;
	/**
	 * Returns if  a specific header exists in the  collection.
	 * @param name The specific header.
	 * @return is the specified header exists in the collection; otherwise .
	 */
	function Contains(name:String):Bool;
	/**
	 * Returns an enumerator that can iterate through the  instance.
	 * @return An enumerator for the .
	 */
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.collections.generic.KeyValuePair_2<String, cs.system.collections.generic.IEnumerable<String>>>;
	/**
	 * Returns all header values for a specified header stored in the  collection.
	 * @param name The specified header to return values for.
	 * @return An array of header strings.
	 */
	function GetValues(name:String):cs.system.collections.generic.IEnumerable<String>;
	/**
	 * Removes the specified header from the  collection.
	 * @param name The name of the header to remove from the collection.
	 * @return Returns .
	 */
	function Remove(name:String):Bool;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current object.
	 */
	function ToString():String;
	@:overload(function(name:String, values:cs.system.collections.generic.IEnumerable<String>):Bool {})
	/**
	 * Returns a value that indicates whether the specified header and its values were
	 * added to the  collection without validating the provided information.
	 * @param name The header to add to the collection.
	 * @param values The values of the header.
	 * @return if the specified header  and  could be added to the collection;
	 * otherwise .
	 */
	function TryAddWithoutValidation(name:String, value:String):Bool;
	/**
	 * Return if a specified header and specified values are stored in the  collection.
	 * @param name The specified header.
	 * @param values The specified header values.
	 * @return is the specified header  and  are stored in the collection; otherwise .
	 */
	function TryGetValues(name:String, values:cs.Ref<cs.system.collections.generic.IEnumerable<String>>):Bool;
}
