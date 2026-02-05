package cs.system.xml;

/** Provides read-only access to a set of prefix and namespace mappings. */
@:native("System.Xml.IXmlNamespaceResolver")
extern interface IXmlNamespaceResolver {
	/**
	 * Gets a collection of defined prefix-namespace mappings that are currently in
	 * scope.
	 * @param scope An  value that specifies the type of namespace nodes to return.
	 * @return An  that contains the current in-scope namespaces.
	 */
	function GetNamespacesInScope(scope:cs.system.xml.XmlNamespaceScope):cs.system.collections.generic.IDictionary<String, String>;
	/**
	 * Gets the namespace URI mapped to the specified prefix.
	 * @param prefix The prefix whose namespace URI you wish to find.
	 * @return The namespace URI that is mapped to the prefix;  if the prefix is not
	 * mapped to a namespace URI.
	 */
	function LookupNamespace(prefix:String):String;
	/**
	 * Gets the prefix that is mapped to the specified namespace URI.
	 * @param namespaceName The namespace URI whose prefix you wish to find.
	 * @return The prefix that is mapped to the namespace URI;  if the namespace URI is
	 * not mapped to a prefix.
	 */
	function LookupPrefix(namespaceName:String):String;
}
