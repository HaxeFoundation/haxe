package cs.system.xml.serialization;

/** Represents a collection of attribute objects that control how the  serializes and deserializes an object. */
@:native("System.Xml.Serialization.XmlAttributes")
extern class XmlAttributes {
	/**
	 * Gets or sets the  to override.
	 * @return The  to override.
	 */
	var XmlAnyAttribute(default, default):cs.system.xml.serialization.XmlAnyAttributeAttribute;
	/**
	 * Gets the collection of  objects to override.
	 * @return An  object that represents the collection of  objects.
	 */
	var XmlAnyElements(default, never):cs.system.xml.serialization.XmlAnyElementAttributes;
	/**
	 * Gets or sets an object that specifies how the  serializes a public field or
	 * read/write property that returns an array.
	 * @return An  that specifies how the  serializes a public field or read/write
	 * property that returns an array.
	 */
	var XmlArray(default, default):cs.system.xml.serialization.XmlArrayAttribute;
	/**
	 * Gets or sets a collection of objects that specify how the  serializes items
	 * inserted into an array returned by a public field or read/write property.
	 * @return An  object that contains a collection of  objects.
	 */
	var XmlArrayItems(default, never):cs.system.xml.serialization.XmlArrayItemAttributes;
	/**
	 * Gets or sets an object that specifies how the  serializes a public field or
	 * public read/write property as an XML attribute.
	 * @return An  that controls the serialization of a public field or read/write
	 * property as an XML attribute.
	 */
	var XmlAttribute(default, default):cs.system.xml.serialization.XmlAttributeAttribute;
	/**
	 * Gets or sets an object that allows you to distinguish between a set of choices.
	 * @return An  that can be applied to a class member that is serialized as an 
	 * element.
	 */
	var XmlChoiceIdentifier(default, never):cs.system.xml.serialization.XmlChoiceIdentifierAttribute;
	/**
	 * Gets or sets the default value of an XML element or attribute.
	 * @return An  that represents the default value of an XML element or attribute.
	 */
	var XmlDefaultValue(default, default):Dynamic;
	/**
	 * Gets a collection of objects that specify how the  serializes a public field or
	 * read/write property as an XML element.
	 * @return An  that contains a collection of  objects.
	 */
	var XmlElements(default, never):cs.system.xml.serialization.XmlElementAttributes;
	/**
	 * Gets or sets an object that specifies how the  serializes an enumeration member.
	 * @return An  that specifies how the  serializes an enumeration member.
	 */
	var XmlEnum(default, default):cs.system.xml.serialization.XmlEnumAttribute;
	/**
	 * Gets or sets a value that specifies whether or not the  serializes a public
	 * field or public read/write property.
	 * @return if the  must not serialize the field or property; otherwise, .
	 */
	var XmlIgnore(default, default):Bool;
	/**
	 * Gets or sets a value that specifies whether to keep all namespace declarations
	 * when an object containing a member that returns an  object is overridden.
	 * @return if the namespace declarations should be kept; otherwise, .
	 */
	var Xmlns(default, default):Bool;
	/**
	 * Gets or sets an object that specifies how the  serializes a class as an XML root
	 * element.
	 * @return An  that overrides a class attributed as an XML root element.
	 */
	var XmlRoot(default, default):cs.system.xml.serialization.XmlRootAttribute;
	/**
	 * Gets or sets an object that instructs the  to serialize a public field or public
	 * read/write property as XML text.
	 * @return An  that overrides the default serialization of a public property or
	 * field.
	 */
	var XmlText(default, default):cs.system.xml.serialization.XmlTextAttribute;
	/**
	 * Gets or sets an object that specifies how the  serializes a class to which the 
	 * has been applied.
	 * @return An  that overrides an  applied to a class declaration.
	 */
	var XmlType(default, default):cs.system.xml.serialization.XmlTypeAttribute;
	@:overload(function():Void {})
	function new(provider:cs.system.reflection.ICustomAttributeProvider):Void;
}
