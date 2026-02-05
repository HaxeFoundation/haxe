package cs.system.xml;

/** Resolves external XML resources named by a Uniform Resource Identifier (URI). */
@:native("System.Xml.XmlResolver")
extern class XmlResolver {
	/**
	 * When overridden in a derived class, sets the credentials used to authenticate
	 * web requests.
	 * @return The credentials to be used to authenticate web requests. If this
	 * property is not set, the value defaults to ; that is, the  has no user
	 * credentials.
	 */
	var Credentials(never, default):cs.system.net.ICredentials;
	/**
	 * When overridden in a derived class, maps a URI to an object that contains the
	 * actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role Currently not used.
	 * @param ofObjectToReturn The type of object to return. The current version only
	 * returns System.IO.Stream objects.
	 * @return A stream object or  if a type other than stream is specified.
	 */
	function GetEntity(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):Dynamic;
	/**
	 * Asynchronously maps a URI to an object that contains the actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role Currently not used.
	 * @param ofObjectToReturn The type of object to return. The current version only
	 * returns  objects.
	 * @return A stream object or  if a type other than stream is specified.
	 */
	function GetEntityAsync(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):cs.system.threading.tasks.Task_1<Dynamic>;
	/**
	 * When overridden in a derived class, resolves the absolute URI from the base and
	 * relative URIs.
	 * @param baseUri The base URI used to resolve the relative URI.
	 * @param relativeUri The URI to resolve. The URI can be absolute or relative. If
	 * absolute, this value effectively replaces the  value. If relative, it combines
	 * with the  to make an absolute URI.
	 * @return The absolute URI or  if the relative URI cannot be resolved.
	 */
	function ResolveUri(baseUri:cs.system.Uri, relativeUri:String):cs.system.Uri;
	/**
	 * Enables the resolver to return types other than .
	 * @param absoluteUri The URI.
	 * @param type The type to return.
	 * @return if the  is supported; otherwise, .
	 */
	function SupportsType(absoluteUri:cs.system.Uri, type:cs.system.Type):Bool;
}
