package cs.system.xml.schema;

/** Abstract class for that is the base class for all particle types (e.g. ). */
@:native("System.Xml.Schema.XmlSchemaParticle")
extern class XmlSchemaParticle extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets the maximum number of times the particle can occur.
	 * @return The maximum number of times the particle can occur. The default is 1.
	 */
	var MaxOccurs(default, default):cs.system.Decimal;
	/**
	 * Gets or sets the number as a string value. Maximum number of times the particle
	 * can occur.
	 * @return The number as a string value.  indicates that  is equal to the default
	 * value. The default is a null reference.
	 */
	var MaxOccursString(default, default):String;
	/**
	 * Gets or sets the minimum number of times the particle can occur.
	 * @return The minimum number of times the particle can occur. The default is 1.
	 */
	var MinOccurs(default, default):cs.system.Decimal;
	/**
	 * Gets or sets the number as a string value. The minimum number of times the
	 * particle can occur.
	 * @return The number as a string value.  indicates that  is equal to the default
	 * value. The default is a null reference.
	 */
	var MinOccursString(default, default):String;
}
