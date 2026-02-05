package cs.system.net.http;

/** Provides a collection of  objects that get serialized using the multipart/* content type specification. */
@:native("System.Net.Http.MultipartContent")
extern class MultipartContent extends cs.system.net.http.HttpContent {
	@:overload(function():Void {})
	@:overload(function(subtype:String):Void {})
	function new(subtype:String, boundary:String):Void;
	/**
	 * Add multipart HTTP content to a collection of  objects that get serialized using
	 * the multipart/* content type specification.
	 * @param content The HTTP content to add to the collection.
	 */
	function Add(content:cs.system.net.http.HttpContent):Void;
	/**
	 * Returns an enumerator that iterates through the collection of  objects that get
	 * serialized using the multipart/* content type specification.
	 * @return An object that can be used to iterate through the collection.
	 */
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.net.http.HttpContent>;
}
