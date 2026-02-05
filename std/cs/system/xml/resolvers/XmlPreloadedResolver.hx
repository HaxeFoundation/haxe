package cs.system.xml.resolvers;

/** Represents a class that is used to prepopulate the cache with DTDs or XML streams. */
@:native("System.Xml.Resolvers.XmlPreloadedResolver")
extern class XmlPreloadedResolver extends cs.system.xml.XmlResolver {
	/**
	 * Gets a collection of preloaded URIs.
	 * @return The collection of preloaded URIs.
	 */
	var PreloadedUris(default, never):cs.system.collections.generic.IEnumerable<cs.system.Uri>;
	@:overload(function():Void {})
	@:overload(function(preloadedDtds:cs.system.xml.resolvers.XmlKnownDtds):Void {})
	@:overload(function(fallbackResolver:cs.system.xml.XmlResolver):Void {})
	@:overload(function(fallbackResolver:cs.system.xml.XmlResolver, preloadedDtds:cs.system.xml.resolvers.XmlKnownDtds):Void {})
	function new(fallbackResolver:cs.system.xml.XmlResolver, preloadedDtds:cs.system.xml.resolvers.XmlKnownDtds, uriComparer:cs.system.collections.generic.IEqualityComparer<cs.system.Uri>):Void;
	@:overload(function(uri:cs.system.Uri, value:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(uri:cs.system.Uri, value:cs.system.io.Stream):Void {})
	@:overload(function(uri:cs.system.Uri, value:String):Void {})
	/**
	 * Adds a byte array to the  store and maps it to a URI. If the store already
	 * contains a mapping for the same URI, the existing mapping is overridden.
	 * @param uri The URI of the data that is being added to the  store.
	 * @param value A byte array with the data that corresponds to the provided URI.
	 */
	function Add(uri:cs.system.Uri, value:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	/**
	 * Maps a URI to an object that contains the actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role The current version of the .NET Framework for Silverlight does not
	 * use this parameter when resolving URIs. This parameter is provided for future
	 * extensibility purposes. For example, this parameter can be mapped to the
	 * xlink:role and used as an implementation-specific argument in other scenarios.
	 * @param ofObjectToReturn The type of object to return. The  supports  objects and
	 * objects for URIs that were added as . If the requested type is not supported by
	 * the resolver, an exception will be thrown. Use the  method to determine whether
	 * a certain  is supported by this resolver.
	 * @return A  or  object that corresponds to the actual source.
	 */
	function GetEntity(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):Dynamic;
	/**
	 * Asynchronously maps a URI to an object that contains the actual resource.
	 * @param absoluteUri The URI returned from .
	 * @param role The current version of the .NET Framework for Silverlight does not
	 * use this parameter when resolving URIs. This parameter is provided for future
	 * extensibility purposes. For example, this parameter can be mapped to the
	 * xlink:role and used as an implementation-specific argument in other scenarios.
	 * @param ofObjectToReturn The type of object to return. The  supports  objects and
	 * objects for URIs that were added as . If the requested type is not supported by
	 * the resolver, an exception will be thrown. Use the  method to determine whether
	 * a certain  is supported by this resolver.
	 * @return A  or  object that corresponds to the actual source.
	 */
	function GetEntityAsync(absoluteUri:cs.system.Uri, role:String, ofObjectToReturn:cs.system.Type):cs.system.threading.tasks.Task_1<Dynamic>;
	/**
	 * Removes the data that corresponds to the URI from the .
	 * @param uri The URI of the data that should be removed from the  store.
	 */
	function Remove(uri:cs.system.Uri):Void;
	/**
	 * Resolves the absolute URI from the base and relative URIs.
	 * @param baseUri The base URI used to resolve the relative URI.
	 * @param relativeUri The URI to resolve. The URI can be absolute or relative. If
	 * absolute, this value effectively replaces the  value. If relative, it combines
	 * with the  to make an absolute URI.
	 * @return The  representing the absolute URI or  if the relative URI cannot be
	 * resolved.
	 */
	function ResolveUri(baseUri:cs.system.Uri, relativeUri:String):cs.system.Uri;
	/**
	 * Determines whether the resolver supports other s than just .
	 * @param absoluteUri The absolute URI to check.
	 * @param type The  to return.
	 * @return if the  is supported; otherwise, .
	 */
	function SupportsType(absoluteUri:cs.system.Uri, type:cs.system.Type):Bool;
}
