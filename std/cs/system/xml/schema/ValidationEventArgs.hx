package cs.system.xml.schema;

/** Returns detailed information related to the . */
@:native("System.Xml.Schema.ValidationEventArgs")
extern class ValidationEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the  associated with the validation event.
	 * @return The  associated with the validation event.
	 */
	var Exception(default, never):cs.system.xml.schema.XmlSchemaException;
	/**
	 * Gets the text description corresponding to the validation event.
	 * @return The text description.
	 */
	var Message(default, never):String;
	/**
	 * Gets the severity of the validation event.
	 * @return An  value representing the severity of the validation event.
	 */
	var Severity(default, never):cs.system.xml.schema.XmlSeverityType;
}
