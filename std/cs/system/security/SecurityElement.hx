package cs.system.security;

/** Represents the XML object model for encoding security objects. This class cannot be inherited. */
@:native("System.Security.SecurityElement")
extern class SecurityElement {
	/**
	 * Gets or sets the attributes of an XML element as name/value pairs.
	 * @return The  object for the attribute values of the XML element.
	 */
	var Attributes(default, default):cs.system.collections.Hashtable;
	/**
	 * Gets or sets the array of child elements of the XML element.
	 * @return The ordered child elements of the XML element as security elements.
	 */
	var Children(default, default):cs.system.collections.ArrayList;
	/**
	 * Gets or sets the tag name of an XML element.
	 * @return The tag name of an XML element.
	 */
	var Tag(default, default):String;
	/**
	 * Gets or sets the text within an XML element.
	 * @return The value of the text within an XML element.
	 */
	var Text(default, default):String;
	@:overload(function(tag:String):Void {})
	function new(tag:String, text:String):Void;
	/**
	 * Replaces invalid XML characters in a string with their valid XML equivalent.
	 * @param str The string within which to escape invalid characters.
	 * @return The input string with invalid characters replaced.
	 */
	static function Escape(str:String):String;
	/**
	 * Creates a security element from an XML-encoded string.
	 * @param xml The XML-encoded string from which to create the security element.
	 * @return A  created from the XML.
	 */
	static function FromString(xml:String):cs.system.security.SecurityElement;
	/**
	 * Determines whether a string is a valid attribute name.
	 * @param name The attribute name to test for validity.
	 * @return if the  parameter is a valid XML attribute name; otherwise, .
	 */
	static function IsValidAttributeName(name:String):Bool;
	/**
	 * Determines whether a string is a valid attribute value.
	 * @param value The attribute value to test for validity.
	 * @return if the  parameter is a valid XML attribute value; otherwise, .
	 */
	static function IsValidAttributeValue(value:String):Bool;
	/**
	 * Determines whether a string is a valid tag.
	 * @param tag The tag to test for validity.
	 * @return if the  parameter is a valid XML tag; otherwise, .
	 */
	static function IsValidTag(tag:String):Bool;
	/**
	 * Determines whether a string is valid as text within an XML element.
	 * @param text The text to test for validity.
	 * @return if the  parameter is a valid XML text element; otherwise, .
	 */
	static function IsValidText(text:String):Bool;
	/**
	 * Adds a name/value attribute to an XML element.
	 * @param name The name of the attribute.
	 * @param value The value of the attribute.
	 */
	function AddAttribute(name:String, value:String):Void;
	/**
	 * Adds a child element to the XML element.
	 * @param child The child element to add.
	 */
	function AddChild(child:cs.system.security.SecurityElement):Void;
	/**
	 * Finds an attribute by name in an XML element.
	 * @param name The name of the attribute for which to search.
	 * @return The value associated with the named attribute, or  if no attribute with 
	 * exists.
	 */
	function Attribute(name:String):String;
	/**
	 * Creates and returns an identical copy of the current  object.
	 * @return A copy of the current  object.
	 */
	function Copy():cs.system.security.SecurityElement;
	/**
	 * Compares two XML element objects for equality.
	 * @param other An XML element object to which to compare the current XML element
	 * object.
	 * @return if the tag, attribute names and values, child elements, and text fields
	 * in the current XML element are identical to their counterparts in the 
	 * parameter; otherwise, .
	 */
	function Equal(other:cs.system.security.SecurityElement):Bool;
	/**
	 * Finds a child by its tag name.
	 * @param tag The tag for which to search in child elements.
	 * @return The first child XML element with the specified tag value, or  if no
	 * child element with  exists.
	 */
	function SearchForChildByTag(tag:String):cs.system.security.SecurityElement;
	/**
	 * Finds a child by its tag name and returns the contained text.
	 * @param tag The tag for which to search in child elements.
	 * @return The text contents of the first child element with the specified tag
	 * value.
	 */
	function SearchForTextOfTag(tag:String):String;
	/**
	 * Produces a string representation of an XML element and its constituent
	 * attributes, child elements, and text.
	 * @return The XML element and its contents.
	 */
	function ToString():String;
}
