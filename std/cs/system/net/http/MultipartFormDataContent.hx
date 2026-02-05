package cs.system.net.http;

/** Provides a container for content encoded using multipart/form-data MIME type. */
@:native("System.Net.Http.MultipartFormDataContent")
extern class MultipartFormDataContent extends cs.system.net.http.MultipartContent {
	@:overload(function():Void {})
	function new(boundary:String):Void;
	@:overload(function(content:cs.system.net.http.HttpContent):Void {})
	@:overload(function(content:cs.system.net.http.HttpContent, name:String):Void {})
	/**
	 * Add HTTP content to a collection of  objects that get serialized to
	 * multipart/form-data MIME type.
	 * @param content The HTTP content to add to the collection.
	 */
	function Add(content:cs.system.net.http.HttpContent, name:String, fileName:String):Void;
}
