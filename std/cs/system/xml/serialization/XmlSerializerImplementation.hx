package cs.system.xml.serialization;

/** Defines the reader, writer, and methods for pre-generated, typed serializers. */
@:native("System.Xml.Serialization.XmlSerializerImplementation")
extern class XmlSerializerImplementation {
	/**
	 * Gets the XML reader object that is used by the serializer.
	 * @return An  that is used to read an XML document or data stream.
	 */
	var Reader(default, never):cs.system.xml.serialization.XmlSerializationReader;
	/**
	 * Gets the collection of methods that is used to read a data stream.
	 * @return A  that contains the methods.
	 */
	var ReadMethods(default, never):cs.system.collections.Hashtable;
	/**
	 * Gets the collection of typed serializers that is found in the assembly.
	 * @return A  that contains the typed serializers.
	 */
	var TypedSerializers(default, never):cs.system.collections.Hashtable;
	/**
	 * Gets the collection of methods that is used to write to a data stream.
	 * @return A  that contains the methods.
	 */
	var WriteMethods(default, never):cs.system.collections.Hashtable;
	/**
	 * Gets the XML writer object for the serializer.
	 * @return An  that is used to write to an XML data stream or document.
	 */
	var Writer(default, never):cs.system.xml.serialization.XmlSerializationWriter;
	/**
	 * Gets a value that determines whether a type can be serialized.
	 * @param type The type to be serialized.
	 * @return if the type can be serialized; otherwise, .
	 */
	function CanSerialize(type:cs.system.Type):Bool;
	/**
	 * Returns a serializer for the specified type.
	 * @param type The type to be serialized.
	 * @return An instance of a type derived from the  class.
	 */
	function GetSerializer(type:cs.system.Type):cs.system.xml.serialization.XmlSerializer;
}
