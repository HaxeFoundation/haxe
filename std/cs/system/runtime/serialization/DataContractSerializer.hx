package cs.system.runtime.serialization;

/** Serializes and deserializes an instance of a type into an XML stream or document using a supplied data contract. This class cannot be inherited. */
@:native("System.Runtime.Serialization.DataContractSerializer")
extern class DataContractSerializer extends cs.system.runtime.serialization.XmlObjectSerializer {
	/**
	 * Gets the component used to dynamically map  declarations to known contract
	 * types.
	 * @return An implementation of the  class.
	 */
	var DataContractResolver(default, never):cs.system.runtime.serialization.DataContractResolver;
	/**
	 * Gets a value that specifies whether to ignore data supplied by an extension of
	 * the class when the class is being serialized or deserialized.
	 * @return to omit the extension data; otherwise, .
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
	 * Gets the maximum number of items in an object graph to serialize or deserialize.
	 * @return The maximum number of items to serialize or deserialize. The default is
	 * .
	 */
	var MaxItemsInObjectGraph(default, never):Int;
	/**
	 * Gets a value that specifies whether to use non-standard XML constructs to
	 * preserve object reference data.
	 * @return to keep the references; otherwise, .
	 */
	var PreserveObjectReferences(default, never):Bool;
	/**
	 * Gets a value that specifies whether read-only types are serialized.
	 * @return if read-only types are serialized;  if all types are serialized.
	 */
	var SerializeReadOnlyTypes(default, never):Bool;
	@:overload(function(type:cs.system.Type):Void {})
	@:overload(function(type:cs.system.Type, knownTypes:cs.system.collections.generic.IEnumerable<cs.system.Type>):Void {})
	@:overload(function(type:cs.system.Type, settings:cs.system.runtime.serialization.DataContractSerializerSettings):Void {})
	@:overload(function(type:cs.system.Type, rootName:String, rootNamespace:String):Void {})
	@:overload(function(type:cs.system.Type, rootName:cs.system.xml.XmlDictionaryString, rootNamespace:cs.system.xml.XmlDictionaryString):Void {})
	@:overload(function(type:cs.system.Type, rootName:String, rootNamespace:String, knownTypes:cs.system.collections.generic.IEnumerable<cs.system.Type>):Void {})
	function new(type:cs.system.Type, rootName:cs.system.xml.XmlDictionaryString, rootNamespace:cs.system.xml.XmlDictionaryString, knownTypes:cs.system.collections.generic.IEnumerable<cs.system.Type>):Void;
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader):Bool {})
	/**
	 * Determines whether the  is positioned on an object that can be deserialized.
	 * @param reader An  used to read the XML stream.
	 * @return if the reader is at the start element of the stream to read; otherwise,
	 * .
	 */
	function IsStartObject(reader:cs.system.xml.XmlReader):Bool;
	@:overload(function(reader:cs.system.xml.XmlReader):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlDictionaryReader, verifyObjectName:Bool):Dynamic {})
	@:overload(function(reader:cs.system.xml.XmlReader, verifyObjectName:Bool):Dynamic {})
	/**
	 * Reads the XML stream with an  and returns the deserialized object, and also
	 * specifies whether a check is made to verify the object name before reading its
	 * value.
	 * @param reader The  used to read the XML stream.
	 * @param verifyObjectName to check whether the name of the object corresponds to
	 * the root name value supplied in the constructor; otherwise, .
	 * @return The deserialized object.
	 */
	function ReadObject(reader:cs.system.xml.XmlDictionaryReader, verifyObjectName:Bool, dataContractResolver:cs.system.runtime.serialization.DataContractResolver):Dynamic;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter):Void {})
	/**
	 * Writes the closing XML element using an .
	 * @param writer The  used to write the stream.
	 */
	function WriteEndObject(writer:cs.system.xml.XmlWriter):Void;
	@:overload(function(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void {})
	/**
	 * Writes all the object data (starting XML element, content, and enclosing
	 * element) to an XML document or stream  using the specified XmlDictionaryWriter.
	 * The method includes a resolver for mapping  declarations at runtime.
	 * @param writer An XmlDictionaryWriter used to write the content to the XML
	 * document or stream.
	 * @param graph The object that contains the content to write.
	 * @param dataContractResolver An implementation of the  used to map  declarations
	 * to known data contracts.
	 */
	function WriteObject(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic, dataContractResolver:cs.system.runtime.serialization.DataContractResolver):Void;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes the XML content using an .
	 * @param writer The  used to write the stream.
	 * @param graph The object to write to the stream.
	 */
	function WriteObjectContent(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
	@:overload(function(writer:cs.system.xml.XmlDictionaryWriter, graph:Dynamic):Void {})
	/**
	 * Writes the opening XML element using an .
	 * @param writer The  used to write the XML start element.
	 * @param graph The object to write.
	 */
	function WriteStartObject(writer:cs.system.xml.XmlWriter, graph:Dynamic):Void;
}
