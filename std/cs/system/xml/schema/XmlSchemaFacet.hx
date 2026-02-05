package cs.system.xml.schema;

/** Abstract class for all facets that are used when simple types are derived by restriction. */
@:native("System.Xml.Schema.XmlSchemaFacet")
extern class XmlSchemaFacet extends cs.system.xml.schema.XmlSchemaAnnotated {
	/**
	 * Gets or sets information that indicates that this facet is fixed.
	 * @return If , value is fixed; otherwise, . The default is . Optional.
	 */
	var IsFixed(default, default):Bool;
	/**
	 * Gets or sets the  attribute of the facet.
	 * @return The value attribute.
	 */
	var Value(default, default):String;
}
