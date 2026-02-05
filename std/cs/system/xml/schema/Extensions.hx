package cs.system.xml.schema;

/** This class contains the LINQ to XML extension methods for XSD validation. */
@:native("System.Xml.Schema.Extensions")
extern class Extensions {
	@:overload(function(source:cs.system.xml.linq.XAttribute):cs.system.xml.schema.IXmlSchemaInfo {})
	/**
	 * Gets the post-schema-validation infoset (PSVI) of a validated attribute.
	 * @param source An  that has been previously validated.
	 * @return A  that contains the post-schema-validation infoset for an .
	 */
	static function GetSchemaInfo(source:cs.system.xml.linq.XElement):cs.system.xml.schema.IXmlSchemaInfo;
	@:overload(function(source:cs.system.xml.linq.XDocument, schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):Void {})
	@:overload(function(source:cs.system.xml.linq.XAttribute, partialValidationType:cs.system.xml.schema.XmlSchemaObject, schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):Void {})
	@:overload(function(source:cs.system.xml.linq.XDocument, schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler, addSchemaInfo:Bool):Void {})
	@:overload(function(source:cs.system.xml.linq.XElement, partialValidationType:cs.system.xml.schema.XmlSchemaObject, schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler):Void {})
	@:overload(function(source:cs.system.xml.linq.XAttribute, partialValidationType:cs.system.xml.schema.XmlSchemaObject, schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler, addSchemaInfo:Bool):Void {})
	/**
	 * This method validates that an  conforms to a specified  and an .
	 * @param source The  to validate.
	 * @param partialValidationType An  that specifies the sub-tree to validate.
	 * @param schemas An  to validate against.
	 * @param validationEventHandler A  for an event that occurs when the reader
	 * encounters validation errors. If , throws an exception upon validation errors.
	 */
	static function Validate(source:cs.system.xml.linq.XElement, partialValidationType:cs.system.xml.schema.XmlSchemaObject, schemas:cs.system.xml.schema.XmlSchemaSet, validationEventHandler:cs.system.xml.schema.ValidationEventHandler, addSchemaInfo:Bool):Void;
}
