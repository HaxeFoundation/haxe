package cs.system.xml.schema;

/** The base class for all simple types and complex types. */
@:native("System.Xml.Schema.XmlSchemaType")
extern class XmlSchemaType extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets the post-compilation object type or the built-in XML Schema Definition
	 * Language (XSD) data type, simpleType element, or complexType element. This is a
	 * post-schema-compilation infoset property.
	 * @return The built-in XSD data type, simpleType element, or complexType element.
	 */
	var BaseSchemaType(default, never):Dynamic;
	/**
	 * Gets the post-compilation value for the base type of this schema type.
	 * @return An  object representing the base type of this schema type.
	 */
	var BaseXmlSchemaType(default, never):cs.system.xml.schema.XmlSchemaType;
	/**
	 * Gets the post-compilation value for the data type of the complex type.
	 * @return The  post-schema-compilation value.
	 */
	var Datatype(default, never):cs.system.xml.schema.XmlSchemaDatatype;
	/**
	 * Gets the post-compilation information on how this element was derived from its
	 * base type.
	 * @return One of the valid  values.
	 */
	var DerivedBy(default, never):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets or sets the final attribute of the type derivation that indicates if
	 * further derivations are allowed.
	 * @return One of the valid  values. The default is .
	 */
	var Final(default, default):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets the post-compilation value of the  property.
	 * @return The post-compilation value of the  property. The default is the 
	 * attribute value of the  element.
	 */
	var FinalResolved(default, never):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets or sets a value indicating if this type has a mixed content model. This
	 * property is only valid in a complex type.
	 * @return if the type has a mixed content model; otherwise, . The default is .
	 */
	var IsMixed(default, default):Bool;
	/**
	 * Gets or sets the name of the type.
	 * @return The name of the type.
	 */
	var Name(default, default):String;
	/**
	 * Gets the qualified name for the type built from the  attribute of this type.
	 * This is a post-schema-compilation property.
	 * @return The  for the type built from the  attribute of this type.
	 */
	var QualifiedName(default, never):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets the  of the type.
	 * @return One of the  values.
	 */
	var TypeCode(default, never):cs.system.xml.schema.XmlTypeCode;
	function new():Void;
	@:overload(function(typeCode:cs.system.xml.schema.XmlTypeCode):cs.system.xml.schema.XmlSchemaComplexType {})
	/**
	 * Returns an  that represents the built-in complex type of the complex type
	 * specified.
	 * @param typeCode One of the  values representing the complex type.
	 * @return The  that represents the built-in complex type.
	 */
	static function GetBuiltInComplexType(qualifiedName:cs.system.xml.XmlQualifiedName):cs.system.xml.schema.XmlSchemaComplexType;
	@:overload(function(typeCode:cs.system.xml.schema.XmlTypeCode):cs.system.xml.schema.XmlSchemaSimpleType {})
	/**
	 * Returns an  that represents the built-in simple type of the specified simple
	 * type.
	 * @param typeCode One of the  values representing the simple type.
	 * @return The  that represents the built-in simple type.
	 */
	static function GetBuiltInSimpleType(qualifiedName:cs.system.xml.XmlQualifiedName):cs.system.xml.schema.XmlSchemaSimpleType;
	/**
	 * Returns a value indicating if the derived schema type specified is derived from
	 * the base schema type specified
	 * @param derivedType The derived  to test.
	 * @param baseType The base  to test the derived  against.
	 * @param except One of the  values representing a type derivation method to
	 * exclude from testing.
	 * @return if the derived type is derived from the base type; otherwise, .
	 */
	static function IsDerivedFrom(derivedType:cs.system.xml.schema.XmlSchemaType, baseType:cs.system.xml.schema.XmlSchemaType, except:cs.system.xml.schema.XmlSchemaDerivationMethod):Bool;
}
