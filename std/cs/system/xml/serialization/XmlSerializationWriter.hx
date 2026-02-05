package cs.system.xml.serialization;

/** Represents an abstract class used for controlling serialization by the  class. */
@:native("System.Xml.Serialization.XmlSerializationWriter")
extern class XmlSerializationWriter extends cs.system.xml.serialization.XmlSerializationGeneratedCode {
	/**
	 * Gets or sets a value that indicates whether the  method is used to write valid
	 * XML.
	 * @return if the  method returns an encoded name; otherwise, .
	 */
	var EscapeName(default, default):Bool;
	/**
	 * Gets or sets a list of XML qualified name objects that contain the namespaces
	 * and prefixes used to produce qualified names in XML documents.
	 * @return An  that contains the namespaces and prefix pairs.
	 */
	var Namespaces(default, default):cs.system.collections.ArrayList;
	/**
	 * Gets or sets the  that is being used by the .
	 * @return The  used by the class instance.
	 */
	var Writer(default, default):cs.system.xml.XmlWriter;
}
