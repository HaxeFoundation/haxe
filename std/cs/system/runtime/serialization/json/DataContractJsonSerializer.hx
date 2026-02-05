package cs.system.runtime.serialization.json;

/** Serializes objects to the JavaScript Object Notation (JSON) and deserializes JSON data to objects. This class cannot be inherited. */
@:native("System.Runtime.Serialization.Json.DataContractJsonSerializer")
extern class DataContractJsonSerializer extends cs.system.runtime.serialization.XmlObjectSerializer {
	/**
	 * Gets the format of the date and time type items in object graph.
	 * @return The format of the date and time type items in object graph.
	 */
	var DateTimeFormat(default, never):cs.system.runtime.serialization.DateTimeFormat;
	/**
	 * Gets or sets the data contract JSON serializer settings to emit type
	 * information.
	 * @return The data contract JSON serializer settings to emit type information.
	 */
	var EmitTypeInformation(default, never):cs.system.runtime.serialization.EmitTypeInformation;
	/**
	 * Gets a value that specifies whether unknown data is ignored on deserialization
	 * and whether the  interface is ignored on serialization.
	 * @return to ignore unknown data and ; otherwise, .
	 */
	var IgnoreExtensionDataObject(default, never):Bool;
	/**
	 * Gets a collection of types that may be present in the object graph serialized
	 * using this instance of the .
	 * @return A  that contains the expected types passed in as known types to the 
	 * constructor.
	 */
	var KnownTypes(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.Type>;
	/**
	 * Gets the maximum number of items in an object graph that the serializer
	 * serializes or deserializes in one read or write call.
	 * @return The maximum number of items to serialize or deserialize.
	 */
	var MaxItemsInObjectGraph(default, never):Int;
	/**
	 * Gets or sets a value that specifies whether to serialize read only types.
	 * @return to serialize read only types; otherwise .
	 */
	var SerializeReadOnlyTypes(default, never):Bool;
	/**
	 * Gets a value that specifies whether to use a simple dictionary format.
	 * @return to use a simple dictionary format; otherwise, .
	 */
	var UseSimpleDictionaryFormat(default, never):Bool;
	@:overload(function(type:cs.system.Type):Void {})
	@:overload(function(type:cs.system.Type, knownTypes:cs.system.collections.generic.IEnumerable<cs.system.Type>):Void {})
	@:overload(function(type:cs.system.Type, settings:cs.system.runtime.serialization.json.DataContractJsonSerializerSettings):Void {})
	@:overload(function(type:cs.system.Type, rootName:String):Void {})
	@:overload(function(type:cs.system.Type, rootName:cs.system.xml.XmlDictionaryString):Void {})
	@:overload(function(type:cs.system.Type, rootName:String, knownTypes:cs.system.collections.generic.IEnumerable<cs.system.Type>):Void {})
	function new(type:cs.system.Type, rootName:cs.system.xml.XmlDictionaryString, knownTypes:cs.system.collections.generic.IEnumerable<cs.system.Type>):Void;
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader):Bool {})
	/**
	 * Gets a value that specifies whether the  is positioned over an XML element that
	 * represents an object the serializer can deserialize from.
	 * @param reader The  used to read the XML stream mapped from JSON.
	 * @return if the reader is positioned correctly; otherwise, .
	 */
	function IsStartObject(reader:cs.system.xml.XmlReader):Bool;
	@:overload(function(stream:cs.system.io.Stream):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlReader):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader, verifyObjectName:Bool):Dynamic {})
	/**
	 * Reads a document stream in the JSON (JavaScript Object Notation) format and
	 * returns the deserialized object.
	 * @param stream The  to be read.
	 * @return The deserialized object.
	 */
	function ReadObject(reader:cs.system.xml.XmlReader, verifyObjectName:Bool):Dynamic;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter):Void {})
	/**
	 * Writes the closing XML element to an XML document, using an , which can be
	 * mapped to JavaScript Object Notation (JSON).
	 * @param writer An  used to write the XML document to map to JSON.
	 */
	function WriteEndObject(writer:cs.system.xml.XmlWriter):Void;
	@:overload(function(stream:cs.system.io.Stream, graph:Dynamic):Void {})
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Serializes a specified object to JavaScript Object Notation (JSON) data and
	 * writes the resulting JSON to a stream.
	 * @param stream The  that is written to.
	 * @param graph The object that contains the data to write to the stream.
	 */
	function WriteObject(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes the XML content that can be mapped to JavaScript Object Notation (JSON)
	 * using an .
	 * @param writer The  to write to.
	 * @param graph The object to write.
	 */
	function WriteObjectContent(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes the opening XML element for serializing an object to XML that can be
	 * mapped to JavaScript Object Notation (JSON) using an .
	 * @param writer The  used to write the XML start element.
	 * @param graph The object to write.
	 */
	function WriteStartObject(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
}
