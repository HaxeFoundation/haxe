package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class is the base class for all particle types and is used to describe an element in an XML document. */
@:native("System.Xml.Schema.XmlSchemaElement")
extern class XmlSchemaElement extends cs.system.xml.schema.XmlSchemaParticle {
	/**
	 * Gets or sets a  derivation.
	 * @return The attribute used to block a type derivation. Default value is .
	 * Optional.
	 */
	var Block(default, default):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets the post-compilation value of the  property.
	 * @return The post-compilation value of the  property. The default is the  value
	 * on the  element.
	 */
	var BlockResolved(default, never):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets the collection of constraints on the element.
	 * @return The collection of constraints.
	 */
	var Constraints(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets or sets the default value of the element if its content is a simple type or
	 * content of the element is .
	 * @return The default value for the element. The default is a null reference.
	 * Optional.
	 */
	var DefaultValue(default, default):String;
	/**
	 * Gets an  object representing the type of the element based on the  or  values of
	 * the element.
	 * @return An  object.
	 */
	var ElementSchemaType(default, never):cs.system.xml.schema.XmlSchemaType;
	/**
	 * Gets a common language runtime (CLR) object based on the  or  of the element,
	 * which holds the post-compilation value of the  property.
	 * @return The common language runtime object. The post-compilation value of the 
	 * property.
	 */
	var ElementType(default, never):Dynamic;
	/**
	 * Gets or sets the  property to indicate that no further derivations are allowed.
	 * @return The  property. The default is . Optional.
	 */
	var Final(default, default):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets the post-compilation value of the  property.
	 * @return The post-compilation value of the  property. Default value is the  value
	 * on the  element.
	 */
	var FinalResolved(default, never):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets or sets the fixed value.
	 * @return The fixed value that is predetermined and unchangeable. The default is a
	 * null reference. Optional.
	 */
	var FixedValue(default, default):String;
	/**
	 * Gets or sets the form for the element.
	 * @return The form for the element. The default is the  value. Optional.
	 */
	var Form(default, default):cs.system.xml.schema.XmlSchemaForm;
	/**
	 * Gets or sets information to indicate if the element can be used in an instance
	 * document.
	 * @return If , the element cannot appear in the instance document. The default is
	 * . Optional.
	 */
	var IsAbstract(default, default):Bool;
	/**
	 * Gets or sets information that indicates if  can occur in the instance data.
	 * Indicates if an explicit nil value can be assigned to the element.
	 * @return If nillable is , this enables an instance of the element to have the 
	 * attribute set to . The  attribute is defined as part of the XML Schema namespace
	 * for instances. The default is . Optional.
	 */
	var IsNillable(default, default):Bool;
	/**
	 * Gets or sets the name of the element.
	 * @return The name of the element. The default is .
	 */
	var Name(default, default):String;
	/**
	 * Gets the actual qualified name for the given element.
	 * @return The qualified name of the element. The post-compilation value of the 
	 * property.
	 */
	var QualifiedName(default, never):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets the reference name of an element declared in this schema (or
	 * another schema indicated by the specified namespace).
	 * @return The reference name of the element.
	 */
	var RefName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets the type of the element. This can either be a complex type or a
	 * simple type.
	 * @return The type of the element.
	 */
	var SchemaType(default, default):cs.system.xml.schema.XmlSchemaType;
	/**
	 * Gets or sets the name of a built-in data type defined in this schema or another
	 * schema indicated by the specified namespace.
	 * @return The name of the built-in data type.
	 */
	var SchemaTypeName(default, default):cs.system.xml.XmlQualifiedName;
	/**
	 * Gets or sets the name of an element that is being substituted by this element.
	 * @return The qualified name of an element that is being substituted by this
	 * element. Optional.
	 */
	var SubstitutionGroup(default, default):cs.system.xml.XmlQualifiedName;
	function new():Void;
}
