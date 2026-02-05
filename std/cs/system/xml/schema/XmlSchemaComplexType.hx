package cs.system.xml.schema;

/** Represents the  element from XML Schema as specified by the World Wide Web Consortium (W3C). This class defines a complex type that determines the set of attributes and content of an element. */
@:native("System.Xml.Schema.XmlSchemaComplexType")
extern class XmlSchemaComplexType extends cs.system.xml.schema.XmlSchemaType {
	/**
	 * Gets or sets the value for the  component of the complex type.
	 * @return The  component of the complex type.
	 */
	var AnyAttribute(default, default):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets the collection of attributes for the complex type.
	 * @return Contains  and  classes.
	 */
	var Attributes(default, never):cs.system.xml.schema.XmlSchemaObjectCollection;
	/**
	 * Gets the collection of all the complied attributes of this complex type and its
	 * base types.
	 * @return The collection of all the attributes from this complex type and its base
	 * types. The post-compilation value of the  property.
	 */
	var AttributeUses(default, never):cs.system.xml.schema.XmlSchemaObjectTable;
	/**
	 * Gets the post-compilation value for  for this complex type and its base type(s).
	 * @return The post-compilation value of the  element.
	 */
	var AttributeWildcard(default, never):cs.system.xml.schema.XmlSchemaAnyAttribute;
	/**
	 * Gets or sets the  attribute.
	 * @return The  attribute prevents a complex type from being used in the specified
	 * type of derivation. The default is . Optional.
	 */
	var Block(default, default):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets the value after the type has been compiled to the post-schema-validation
	 * information set (infoset). This value indicates how the type is enforced when 
	 * is used in the instance document.
	 * @return The post-schema-validated infoset value. The default is  value on the 
	 * element.
	 */
	var BlockResolved(default, never):cs.system.xml.schema.XmlSchemaDerivationMethod;
	/**
	 * Gets or sets the post-compilation  of this complex type.
	 * @return The content model type that is one of the  or  classes.
	 */
	var ContentModel(default, default):cs.system.xml.schema.XmlSchemaContentModel;
	/**
	 * Gets the content model of the complex type which holds the post-compilation
	 * value.
	 * @return The post-compilation value of the content model for the complex type.
	 */
	var ContentType(default, never):cs.system.xml.schema.XmlSchemaContentType;
	/**
	 * Gets the particle that holds the post-compilation value of the  particle.
	 * @return The particle for the content type. The post-compilation value of the 
	 * particle.
	 */
	var ContentTypeParticle(default, never):cs.system.xml.schema.XmlSchemaParticle;
	/**
	 * Gets or sets the information that determines if the  element can be used in the
	 * instance document.
	 * @return If , an element cannot use this  element directly and must use a complex
	 * type that is derived from this  element. The default is . Optional.
	 */
	var IsAbstract(default, default):Bool;
	/**
	 * Gets or sets the compositor type as one of the , , , or  classes.
	 * @return The compositor type.
	 */
	var Particle(default, default):cs.system.xml.schema.XmlSchemaParticle;
	function new():Void;
}
