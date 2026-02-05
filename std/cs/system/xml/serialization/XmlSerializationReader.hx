package cs.system.xml.serialization;

/** Controls deserialization by the  class. */
@:native("System.Xml.Serialization.XmlSerializationReader")
extern class XmlSerializationReader extends cs.system.xml.serialization.XmlSerializationGeneratedCode {
	/**
	 * Gets or sets a value that determines whether XML strings are translated into
	 * valid .NET Framework type names.
	 * @return if XML strings are decoded into valid .NET Framework type names;
	 * otherwise, .
	 */
	var DecodeName(default, default):Bool;
	/**
	 * Gets the XML document object into which the XML document is being deserialized.
	 * @return An  that represents the deserialized  data.
	 */
	var Document(default, never):cs.system.xml.XmlDocument;
	/**
	 * Gets or sets a value that should be  for a SOAP 1.1 return value.
	 * @return , if the value is a return value.
	 */
	var IsReturnValue(default, default):Bool;
	/**
	 * Gets the  object that is being used by .
	 * @return The  that is being used by the .
	 */
	var Reader(default, never):cs.system.xml.XmlReader;
	/**
	 * Gets the current count of the .
	 * @return The current count of an .
	 */
	var ReaderCount(default, never):Int;
}
