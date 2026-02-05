package cs.system.xml.schema;

/** Infers an XML Schema Definition Language (XSD) schema from an XML document. The  class cannot be inherited. */
@:native("System.Xml.Schema.XmlSchemaInference")
extern class XmlSchemaInference {
	/**
	 * Gets or sets the  value that affects schema occurrence declarations inferred
	 * from the XML document.
	 * @return An  object.
	 */
	var Occurrence(default, default):cs.system.xml.schema.XmlSchemaInference_InferenceOption;
	/**
	 * Gets or sets the  value that affects types inferred from the XML document.
	 * @return An  object.
	 */
	var TypeInference(default, default):cs.system.xml.schema.XmlSchemaInference_InferenceOption;
	function new():Void;
	@:overload(function(instanceDocument:cs.system.xml.XmlReader):cs.system.xml.schema.XmlSchemaSet {})
	/**
	 * Infers an XML Schema Definition Language (XSD) schema from the XML document
	 * contained in the  object specified.
	 * @param instanceDocument An  object containing the XML document to infer a schema
	 * from.
	 * @return An  object containing the inferred schemas.
	 */
	function InferSchema(instanceDocument:cs.system.xml.XmlReader, schemas:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.schema.XmlSchemaSet;
}
