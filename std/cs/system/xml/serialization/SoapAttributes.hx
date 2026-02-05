package cs.system.xml.serialization;

/** Represents a collection of attribute objects that control how the  serializes and deserializes SOAP methods. */
@:native("System.Xml.Serialization.SoapAttributes")
extern class SoapAttributes {
	/**
	 * Gets or sets the  to override.
	 * @return A  that overrides the behavior of the  when the member is serialized.
	 */
	var SoapAttribute(default, default):cs.system.xml.serialization.SoapAttributeAttribute;
	/**
	 * Gets or sets the default value of an XML element or attribute.
	 * @return An object that represents the default value of an XML element or
	 * attribute.
	 */
	var SoapDefaultValue(default, default):Dynamic;
	/**
	 * Gets or sets a  to override.
	 * @return The  to override.
	 */
	var SoapElement(default, default):cs.system.xml.serialization.SoapElementAttribute;
	/**
	 * Gets or sets an object that specifies how the  serializes a SOAP enumeration.
	 * @return An object that specifies how the  serializes an enumeration member.
	 */
	var SoapEnum(default, default):cs.system.xml.serialization.SoapEnumAttribute;
	/**
	 * Gets or sets a value that specifies whether the  serializes a public field or
	 * property as encoded SOAP XML.
	 * @return if the  must not serialize the field or property; otherwise, .
	 */
	var SoapIgnore(default, default):Bool;
	/**
	 * Gets or sets an object that instructs the  how to serialize an object type into
	 * encoded SOAP XML.
	 * @return A  that either overrides a  applied to a class declaration, or is
	 * applied to a class declaration.
	 */
	var SoapType(default, default):cs.system.xml.serialization.SoapTypeAttribute;
	@:overload(function():Void {})
	function new(provider:cs.system.reflection.ICustomAttributeProvider):Void;
}
