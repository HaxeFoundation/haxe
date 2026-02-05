package cs.system.runtime.serialization;

/** Contains methods for reading and writing XML. */
@:native("System.Runtime.Serialization.XmlSerializableServices")
extern class XmlSerializableServices {
	/**
	 * Generates a default schema type given the specified type name and adds it to the
	 * specified schema set.
	 * @param schemas An  to add the generated schema type to.
	 * @param typeQName An  that specifies the type name to assign the schema to.
	 */
	static function AddDefaultSchema(schemas:cs.system.xml.schema.XmlSchemaSet, typeQName:cs.system.xml.XmlQualifiedName):Void;
	/**
	 * Reads a set of XML nodes from the specified reader and returns the result.
	 * @param xmlReader An  used for reading.
	 * @return An array of type .
	 */
	static function ReadNodes(xmlReader:cs.system.xml.XmlReader):cs.NativeArray<cs.system.xml.XmlNode>;
	/**
	 * Writes the supplied nodes using the specified writer.
	 * @param xmlWriter An  used for writing.
	 * @param nodes An array of type  to write.
	 */
	static function WriteNodes(xmlWriter:cs.system.xml.XmlWriter, nodes:cs.NativeArray<cs.system.xml.XmlNode>):Void;
}
