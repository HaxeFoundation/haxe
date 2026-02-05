package cs.system.xml.serialization;

/** Serializes and deserializes objects into and from XML documents. The  enables you to control how objects are encoded into XML. */
@:native("System.Xml.Serialization.XmlSerializer")
extern class XmlSerializer {
	@:overload(function(type:cs.system.Type):Void {})
	@:overload(function(xmlTypeMapping:cs.system.xml.serialization.XmlTypeMapping):Void {})
	@:overload(function(type:cs.system.Type, defaultNamespace:String):Void {})
	@:overload(function(type:cs.system.Type, extraTypes:cs.NativeArray<cs.system.Type>):Void {})
	@:overload(function(type:cs.system.Type, overrides:cs.system.xml.serialization.XmlAttributeOverrides):Void {})
	@:overload(function(type:cs.system.Type, root:cs.system.xml.serialization.XmlRootAttribute):Void {})
	@:overload(function(type:cs.system.Type, overrides:cs.system.xml.serialization.XmlAttributeOverrides, extraTypes:cs.NativeArray<cs.system.Type>, root:cs.system.xml.serialization.XmlRootAttribute, defaultNamespace:String):Void {})
	function new(type:cs.system.Type, overrides:cs.system.xml.serialization.XmlAttributeOverrides, extraTypes:cs.NativeArray<cs.system.Type>, root:cs.system.xml.serialization.XmlRootAttribute, defaultNamespace:String, location:String):Void;
	@:overload(function(mappings:cs.NativeArray<cs.system.xml.serialization.XmlMapping>):cs.NativeArray<cs.system.xml.serialization.XmlSerializer> {})
	/**
	 * Returns an array of  objects created from an array of  objects.
	 * @param mappings An array of  that maps one type to another.
	 * @return An array of  objects.
	 */
	static function FromMappings(mappings:cs.NativeArray<cs.system.xml.serialization.XmlMapping>, type:cs.system.Type):cs.NativeArray<cs.system.xml.serialization.XmlSerializer>;
	/**
	 * Returns an array of  objects created from an array of types.
	 * @param types An array of  objects.
	 * @return An array of  objects.
	 */
	static function FromTypes(types:cs.NativeArray<cs.system.Type>):cs.NativeArray<cs.system.xml.serialization.XmlSerializer>;
	@:overload(function(type:cs.system.Type):String {})
	/**
	 * Returns the name of the assembly that contains one or more versions of the 
	 * especially created to serialize or deserialize the specified type.
	 * @param type The  you are deserializing.
	 * @return The name of the assembly that contains an  for the type.
	 */
	static function GetXmlSerializerAssemblyName(type:cs.system.Type, defaultNamespace:String):String;
	/**
	 * Gets a value that indicates whether this  can deserialize a specified XML
	 * document.
	 * @param xmlReader An  that points to the document to deserialize.
	 * @return if this  can deserialize the object that the  points to; otherwise, .
	 */
	function CanDeserialize(xmlReader:cs.system.xml.XmlReader):Bool;
	@:overload(function(stream:cs.system.io.Stream):Dynamic {})
	@:overload(function(textReader:cs.system.io.TextReader):Dynamic {})
	@:overload(function(xmlReader:cs.system.xml.XmlReader):Dynamic {})
	@:overload(function(xmlReader:cs.system.xml.XmlReader, encodingStyle:String):Dynamic {})
	@:overload(function(xmlReader:cs.system.xml.XmlReader, events:cs.system.xml.serialization.XmlDeserializationEvents):Dynamic {})
	/**
	 * Deserializes the XML document contained by the specified .
	 * @param stream The  that contains the XML document to deserialize.
	 * @return The  being deserialized.
	 */
	function Deserialize(xmlReader:cs.system.xml.XmlReader, encodingStyle:String, events:cs.system.xml.serialization.XmlDeserializationEvents):Dynamic;
	@:overload(function(stream:cs.system.io.Stream, o:Dynamic):Void {})
	@:overload(function(textWriter:cs.system.io.TextWriter, o:Dynamic):Void {})
	@:overload(function(xmlWriter:cs.system.xml.XmlWriter, o:Dynamic):Void {})
	@:overload(function(stream:cs.system.io.Stream, o:Dynamic, namespaces:cs.system.xml.serialization.XmlSerializerNamespaces):Void {})
	@:overload(function(textWriter:cs.system.io.TextWriter, o:Dynamic, namespaces:cs.system.xml.serialization.XmlSerializerNamespaces):Void {})
	@:overload(function(xmlWriter:cs.system.xml.XmlWriter, o:Dynamic, namespaces:cs.system.xml.serialization.XmlSerializerNamespaces):Void {})
	@:overload(function(xmlWriter:cs.system.xml.XmlWriter, o:Dynamic, namespaces:cs.system.xml.serialization.XmlSerializerNamespaces, encodingStyle:String):Void {})
	/**
	 * Serializes the specified  and writes the XML document to a file using the
	 * specified .
	 * @param stream The  used to write the XML document.
	 * @param o The  to serialize.
	 */
	function Serialize(xmlWriter:cs.system.xml.XmlWriter, o:Dynamic, namespaces:cs.system.xml.serialization.XmlSerializerNamespaces, encodingStyle:String, id:String):Void;
}
