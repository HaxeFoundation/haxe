package cs.system.xml.xsl;

/** Contains a variable number of arguments which are either XSLT parameters or extension objects. */
@:native("System.Xml.Xsl.XsltArgumentList")
extern class XsltArgumentList {
	function new():Void;
	/**
	 * Adds a new object to the  and associates it with the namespace URI.
	 * @param namespaceUri The namespace URI to associate with the object. To use the
	 * default namespace, specify an empty string.
	 * @param extension The object to add to the list.
	 */
	function AddExtensionObject(namespaceUri:String, extension:Dynamic):Void;
	/**
	 * Adds a parameter to the  and associates it with the namespace qualified name.
	 * @param name The name to associate with the parameter.
	 * @param namespaceUri The namespace URI to associate with the parameter. To use
	 * the default namespace, specify an empty string.
	 * @param parameter The parameter value or object to add to the list.
	 */
	function AddParam(name:String, namespaceUri:String, parameter:Dynamic):Void;
	/** Removes all parameters and extension objects from the . */
	function Clear():Void;
	/**
	 * Gets the object associated with the given namespace.
	 * @param namespaceUri The namespace URI of the object.
	 * @return The namespace URI object or  if one was not found.
	 */
	function GetExtensionObject(namespaceUri:String):Dynamic;
	/**
	 * Gets the parameter associated with the namespace qualified name.
	 * @param name The name of the parameter.  does not check to ensure the name passed
	 * is a valid local name; however, the name cannot be .
	 * @param namespaceUri The namespace URI associated with the parameter.
	 * @return The parameter object or  if one was not found.
	 */
	function GetParam(name:String, namespaceUri:String):Dynamic;
	/**
	 * Removes the object with the namespace URI from the .
	 * @param namespaceUri The namespace URI associated with the object to remove.
	 * @return The object with the namespace URI or  if one was not found.
	 */
	function RemoveExtensionObject(namespaceUri:String):Dynamic;
	/**
	 * Removes the parameter from the .
	 * @param name The name of the parameter to remove.  does not check to ensure the
	 * name passed is a valid local name; however, the name cannot be .
	 * @param namespaceUri The namespace URI of the parameter to remove.
	 * @return The parameter object or  if one was not found.
	 */
	function RemoveParam(name:String, namespaceUri:String):Dynamic;
}
