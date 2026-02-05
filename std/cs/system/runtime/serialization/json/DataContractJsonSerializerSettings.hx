package cs.system.runtime.serialization.json;

/** Specifies  settings. */
@:native("System.Runtime.Serialization.Json.DataContractJsonSerializerSettings")
extern class DataContractJsonSerializerSettings {
	/**
	 * Gets or sets a DateTimeFormat that defines the culturally appropriate format of
	 * displaying dates and times.
	 * @return The DateTimeFormat that defines the culturally appropriate format of
	 * displaying dates and times.
	 */
	var DateTimeFormat(default, default):cs.system.runtime.serialization.DateTimeFormat;
	/**
	 * Gets or sets the data contract JSON serializer settings to emit type
	 * information.
	 * @return The data contract JSON serializer settings to emit type information.
	 */
	var EmitTypeInformation(default, default):cs.system.runtime.serialization.EmitTypeInformation;
	/**
	 * Gets or sets a value that specifies whether to ignore data supplied by an
	 * extension of the class when the class is being serialized or deserialized.
	 * @return to ignore data supplied by an extension of the class when the class is
	 * being serialized or deserialized; otherwise, .
	 */
	var IgnoreExtensionDataObject(default, default):Bool;
	/**
	 * Gets or sets a collection of types that may be present in the object graph
	 * serialized using this instance the DataContractJsonSerializerSettings.
	 * @return A collection of types that may be present in the object graph serialized
	 * using this instance the DataContractJsonSerializerSettings.
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
	 * Gets or sets the root name of the selected object.
	 * @return The root name of the selected object.
	 */
	var RootName(default, default):String;
	/**
	 * Gets or sets a value that specifies whether to serialize read only types.
	 * @return to serialize read only types; otherwise .
	 */
	var SerializeReadOnlyTypes(default, default):Bool;
	/**
	 * Gets or sets a value that specifies whether to use a simple dictionary format.
	 * @return to use a simple dictionary format; otherwise, .
	 */
	var UseSimpleDictionaryFormat(default, default):Bool;
	function new():Void;
}
