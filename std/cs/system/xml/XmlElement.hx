package cs.system.xml;

/** Represents an element. */
@:native("System.Xml.XmlElement")
extern class XmlElement extends cs.system.xml.XmlLinkedNode {
	/**
	 * Gets a  value indicating whether the current node has any attributes.
	 * @return if the current node has attributes; otherwise, .
	 */
	var HasAttributes(default, never):Bool;
	/**
	 * Gets or sets the tag format of the element.
	 * @return if the element is to be serialized in the short tag format "<item/>"; 
	 * for the long format "<item></item>". When setting this property, if set to , the
	 * children of the element are removed and the element is serialized in the short
	 * tag format. If set to , the value of the property is changed (regardless of
	 * whether or not the element has content); if the element is empty, it is
	 * serialized in the long format. This property is a Microsoft extension to the
	 * Document Object Model (DOM).
	 */
	var IsEmpty(default, default):Bool;
	/**
	 * Creates a duplicate of this node.
	 * @param deep to recursively clone the subtree under the specified node;  to clone
	 * only the node itself (and its attributes if the node is an ).
	 * @return The cloned node.
	 */
	function CloneNode(deep:Bool):cs.system.xml.XmlNode;
	@:overload(function(name:String):String {})
	/**
	 * Returns the value for the attribute with the specified name.
	 * @param name The name of the attribute to retrieve. This is a qualified name. It
	 * is matched against the  property of the matching node.
	 * @return The value of the specified attribute. An empty string is returned if a
	 * matching attribute is not found or if the attribute does not have a specified or
	 * default value.
	 */
	function GetAttribute(localName:String, namespaceURI:String):String;
	@:overload(function(name:String):cs.system.xml.XmlAttribute {})
	/**
	 * Returns the  with the specified name.
	 * @param name The name of the attribute to retrieve. This is a qualified name. It
	 * is matched against the  property of the matching node.
	 * @return The specified  or  if a matching attribute was not found.
	 */
	function GetAttributeNode(localName:String, namespaceURI:String):cs.system.xml.XmlAttribute;
	@:overload(function(name:String):cs.system.xml.XmlNodeList {})
	/**
	 * Returns an  containing a list of all descendant elements that match the
	 * specified .
	 * @param name The name tag to match. This is a qualified name. It is matched
	 * against the  property of the matching node. The asterisk (*) is a special value
	 * that matches all tags.
	 * @return An  containing a list of all matching nodes. The list is empty if there
	 * are no matching nodes.
	 */
	function GetElementsByTagName(localName:String, namespaceURI:String):cs.system.xml.XmlNodeList;
	@:overload(function(name:String):Bool {})
	/**
	 * Determines whether the current node has an attribute with the specified name.
	 * @param name The name of the attribute to find. This is a qualified name. It is
	 * matched against the  property of the matching node.
	 * @return if the current node has the specified attribute; otherwise, .
	 */
	function HasAttribute(localName:String, namespaceURI:String):Bool;
	/** Removes all specified attributes and children of the current node. Default attributes are not removed. */
	function RemoveAll():Void;
	/** Removes all specified attributes from the element. Default attributes are not removed. */
	function RemoveAllAttributes():Void;
	@:overload(function(name:String):Void {})
	/**
	 * Removes an attribute by name.
	 * @param name The name of the attribute to remove.This is a qualified name. It is
	 * matched against the  property of the matching node.
	 */
	function RemoveAttribute(localName:String, namespaceURI:String):Void;
	/**
	 * Removes the attribute node with the specified index from the element. (If the
	 * removed attribute has a default value, it is immediately replaced).
	 * @param i The index of the node to remove. The first node has index 0.
	 * @return The attribute node removed or  if there is no node at the given index.
	 */
	function RemoveAttributeAt(i:Int):cs.system.xml.XmlNode;
	@:overload(function(oldAttr:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute {})
	/**
	 * Removes the  specified by the local name and namespace URI. (If the removed
	 * attribute has a default value, it is immediately replaced).
	 * @param localName The local name of the attribute.
	 * @param namespaceURI The namespace URI of the attribute.
	 * @return The removed  or  if the  does not have a matching attribute node.
	 */
	function RemoveAttributeNode(localName:String, namespaceURI:String):cs.system.xml.XmlAttribute;
	@:overload(function(name:String, value:String):Void {})
	/**
	 * Sets the value of the attribute with the specified name.
	 * @param name The name of the attribute to create or alter. This is a qualified
	 * name. If the name contains a colon it is parsed into prefix and local name
	 * components.
	 * @param value The value to set for the attribute.
	 */
	function SetAttribute(localName:String, namespaceURI:String, value:String):String;
	@:overload(function(newAttr:cs.system.xml.XmlAttribute):cs.system.xml.XmlAttribute {})
	/**
	 * Adds the specified .
	 * @param localName The local name of the attribute.
	 * @param namespaceURI The namespace URI of the attribute.
	 * @return The  to add.
	 */
	function SetAttributeNode(localName:String, namespaceURI:String):cs.system.xml.XmlAttribute;
	/**
	 * Saves all the children of the node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteContentTo(w:cs.system.xml.XmlWriter):Void;
	/**
	 * Saves the current node to the specified .
	 * @param w The  to which you want to save.
	 */
	function WriteTo(w:cs.system.xml.XmlWriter):Void;
}
