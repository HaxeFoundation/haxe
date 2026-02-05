package cs.system.xml.serialization;

/** Provides custom formatting for XML serialization and deserialization. */
@:native("System.Xml.Serialization.IXmlSerializable")
extern interface IXmlSerializable {
	/**
	 * This method is reserved and should not be used. When implementing the 
	 * interface, you should return  ( in Visual Basic) from this method, and instead,
	 * if specifying a custom schema is required, apply the  to the class.
	 * @return An  that describes the XML representation of the object that is produced
	 * by the  method and consumed by the  method.
	 */
	function GetSchema():cs.system.xml.schema.XmlSchema;
	/**
	 * Generates an object from its XML representation.
	 * @param reader The  stream from which the object is deserialized.
	 */
	function ReadXml(reader:cs.system.xml.XmlReader):Void;
	/**
	 * Converts an object into its XML representation.
	 * @param writer The  stream to which the object is serialized.
	 */
	function WriteXml(writer:cs.system.xml.XmlWriter):Void;
}
