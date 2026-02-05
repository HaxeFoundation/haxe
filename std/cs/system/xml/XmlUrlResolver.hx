package cs.system.xml;

/** Resolves external XML resources named by a Uniform Resource Identifier (URI). */
@:native("System.Xml.XmlUrlResolver")
extern class XmlUrlResolver extends cs.system.xml.XmlResolver {
	/**
	 * Gets or sets the cache policy for the underlying  object.
	 * @return The cache policy for the underlying web request.
	 */
	var CachePolicy(never, default):cs.system.net.cache.RequestCachePolicy;
	/**
	 * Gets or sets the network proxy for the underlying  object.
	 * @return The  to use to access the Internet resource.
	 */
	var Proxy(never, default):cs.system.net.IWebProxy;
	function new():Void;
	/**
	 * Maps a URI to an object that contains the actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role Currently not used.
	 * @param ofObjectToReturn The type of object to return. The current implementation
	 * only returns  objects.
	 * @return A stream object or  if a type other than stream is specified.
	 */
	function GetEntity(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):Dynamic;
	/**
	 * Asynchronously maps a URI to an object that contains the actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role Currently not used.
	 * @param ofObjectToReturn The type of object to return. The current implementation
	 * only returns  objects.
	 * @return A stream object or  if a type other than stream is specified.
	 */
	function GetEntityAsync(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):cs.system.threading.tasks.Task_1<Dynamic>;
	/**
	 * Resolves the absolute URI from the base and relative URIs.
	 * @param baseUri The base URI used to resolve the relative URI.
	 * @param relativeUri The URI to resolve. The URI can be absolute or relative. If
	 * absolute, this value effectively replaces the  value. If relative, it combines
	 * with the  to make an absolute URI.
	 * @return The absolute URI, or  if the relative URI cannot be resolved.
	 */
	function ResolveUri(baseUri:cs.system.Uri, relativeUri:String):cs.system.Uri;
}
