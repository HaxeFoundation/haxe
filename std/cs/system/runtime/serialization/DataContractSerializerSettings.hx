package cs.system.runtime.serialization;

/** Specifies data contract serializer settings. */
@:native("System.Runtime.Serialization.DataContractSerializerSettings")
extern class DataContractSerializerSettings {
	/**
	 * Gets or sets the component used to dynamically map xsi:type declarations to
	 * known contract types.
	 * @return The component used to dynamically map xsi:type declarations to known
	 * contract types.
	 */
	var DataContractResolver(default, default):cs.system.runtime.serialization.DataContractResolver;
	/**
	 * Gets or sets a value that specifies whether to ignore data supplied by an
	 * extension of the class when the class is being serialized or deserialized.
	 * @return to ignore data supplied by an extension of the class when the class is
	 * being serialized or deserialized; otherwise, .
	 */
	var IgnoreExtensionDataObject(default, default):Bool;
	/**
	 * Gets or sets a collection of types that may be present in the object graph
	 * serialized using this instance of the DataContractSerializerSettings.
	 * @return A collection of types that may be present in the object graph serialized
	 * using this instance of the DataContractSerializerSettings.
	 */
	var KnownTypes(default, default):cs.system.collections.generic.IEnumerable<cs.system.Type>;
	/**
	 * Gets or sets the maximum number of items in an object graph to serialize or
	 * deserialize.
	 * @return The maximum number of items in an object graph to serialize or
	 * deserialize.
	 */
	var MaxItemsInObjectGraph(default, default):Int;
	/**
	 * Gets or sets a value that specifies whether to use non-standard XML constructs
	 * to preserve object reference data.
	 * @return to use non-standard XML constructs to preserve object reference data;
	 * otherwise, .
	 */
	var PreserveObjectReferences(default, default):Bool;
	/**
	 * Gets or sets the root name of the selected object.
	 * @return The root name of the selected object.
	 */
	var RootName(default, default):cs.system.xml.XmlDictionaryString;
	/**
	 * Gets or sets the root namespace for the specified object.
	 * @return The root namespace for the specified object.
	 */
	var RootNamespace(default, default):cs.system.xml.XmlDictionaryString;
	/**
	 * Gets or sets a value that specifies whether to serialize read only types.
	 * @return to serialize read only types; otherwise, .
	 */
	var SerializeReadOnlyTypes(default, default):Bool;
	function new():Void;
}
