package cs.system.xml;

/** Helps to secure another implementation of  by wrapping the  object and restricting the resources that the underlying  has access to. */
@:native("System.Xml.XmlSecureResolver")
extern class XmlSecureResolver extends cs.system.xml.XmlResolver {
	function new(resolver:cs.system.xml.XmlResolver, securityUrl:String):Void;
	/**
	 * Maps a URI to an object that contains the actual resource. This method
	 * temporarily sets the  created in the constructor by calling  before calling  on
	 * the underlying  to open the resource.
	 * @param absoluteUri The URI that is returned from .
	 * @param role Currently not used.
	 * @param ofObjectToReturn The type of object to return. The current version only
	 * returns  objects.
	 * @return The stream returned by calling  on the underlying . If a type other than
	 * is specified, the method returns .
	 */
	function GetEntity(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):Dynamic;
	/**
	 * Asynchronously maps a URI to an object that contains the actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role Currently not used.
	 * @param ofObjectToReturn The type of object to return. The current version only
	 * returns  objects.
	 * @return The stream returned by calling  on the underlying . If a type other than
	 * is specified, the method returns .
	 */
	function GetEntityAsync(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):cs.system.threading.tasks.Task_1<Dynamic>;
	/**
	 * Resolves the absolute URI from the base and relative URIs by calling  on the
	 * underlying .
	 * @param baseUri The base URI used to resolve the relative URI.
	 * @param relativeUri The URI to resolve. The URI can be absolute or relative. If
	 * absolute, this value effectively replaces the  value. If relative, it combines
	 * with the  to make an absolute URI.
	 * @return The absolute URI or  if the relative URI cannot be resolved (returned by
	 * calling  on the underlying ).
	 */
	function ResolveUri(baseUri:cs.system.Uri, relativeUri:String):cs.system.Uri;
}
