package cs.system.xml;

/** Resolves, adds, and removes namespaces to a collection and provides scope management for these namespaces. */
@:native("System.Xml.XmlNamespaceManager")
extern class XmlNamespaceManager {
	/**
	 * Gets the namespace URI for the default namespace.
	 * @return The namespace URI for the default namespace, or an empty string if there
	 * is no default namespace.
	 */
	var DefaultNamespace(default, never):String;
	/**
	 * Gets the  associated with this object.
	 * @return The  used by this object.
	 */
	var NameTable(default, never):cs.system.xml.XmlNameTable;
	function new(nameTable:cs.system.xml.XmlNameTable):Void;
	/**
	 * Adds the given namespace to the collection.
	 * @param prefix The prefix to associate with the namespace being added. Use
	 * String.Empty to add a default namespace. NoteIf the  will be used for resolving
	 * namespaces in an XML Path Language (XPath) expression, a prefix must be
	 * specified. If an XPath expression does not include a prefix, it is assumed that
	 * the namespace Uniform Resource Identifier (URI) is the empty namespace. For more
	 * information about XPath expressions and the , refer to the  and  methods.
	 * @param uri The namespace to add.
	 */
	function AddNamespace(prefix:String, uri:String):Void;
	/**
	 * Returns an enumerator to use to iterate through the namespaces in the .
	 * @return An  containing the prefixes stored by the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets a collection of namespace names keyed by prefix which can be used to
	 * enumerate the namespaces currently in scope.
	 * @param scope An enumeration value that specifies the type of namespace nodes to
	 * return.
	 * @return A collection of namespace and prefix pairs currently in scope.
	 */
	function GetNamespacesInScope(scope:cs.system.xml.XmlNamespaceScope):cs.system.collections.generic.IDictionary<String, String>;
	/**
	 * Gets a value indicating whether the supplied prefix has a namespace defined for
	 * the current pushed scope.
	 * @param prefix The prefix of the namespace you want to find.
	 * @return if there is a namespace defined; otherwise, .
	 */
	function HasNamespace(prefix:String):Bool;
	/**
	 * Gets the namespace URI for the specified prefix.
	 * @param prefix The prefix whose namespace URI you want to resolve. To match the
	 * default namespace, pass String.Empty.
	 * @return The namespace URI for  or  if there is no mapped namespace. The returned
	 * string is atomized. For more information on atomized strings, see the  class.
	 */
	function LookupNamespace(prefix:String):String;
	/**
	 * Finds the prefix declared for the given namespace URI.
	 * @param uri The namespace to resolve for the prefix.
	 * @return The matching prefix. If there is no mapped prefix, the method returns
	 * String.Empty. If a null value is supplied, then  is returned.
	 */
	function LookupPrefix(uri:String):String;
	/**
	 * Pops a namespace scope off the stack.
	 * @return if there are namespace scopes left on the stack;  if there are no more
	 * namespaces to pop.
	 */
	function PopScope():Bool;
	/** Pushes a namespace scope onto the stack. */
	function PushScope():Void;
	/**
	 * Removes the given namespace for the given prefix.
	 * @param prefix The prefix for the namespace
	 * @param uri The namespace to remove for the given prefix. The namespace removed
	 * is from the current namespace scope. Namespaces outside the current scope are
	 * ignored.
	 */
	function RemoveNamespace(prefix:String, uri:String):Void;
}
