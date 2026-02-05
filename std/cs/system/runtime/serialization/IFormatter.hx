package cs.system.runtime.serialization;

/** Provides functionality for formatting serialized objects. */
@:native("System.Runtime.Serialization.IFormatter")
extern interface IFormatter {
	/**
	 * Gets or sets the  that performs type lookups during deserialization.
	 * @return The  that performs type lookups during deserialization.
	 */
	var Binder(default, default):cs.system.runtime.serialization.SerializationBinder;
	/**
	 * Gets or sets the  used for serialization and deserialization.
	 * @return The  used for serialization and deserialization.
	 */
	var Context(default, default):cs.system.runtime.serialization.StreamingContext;
	/**
	 * Gets or sets the  used by the current formatter.
	 * @return The  used by this formatter.
	 */
	var SurrogateSelector(default, default):cs.system.runtime.serialization.ISurrogateSelector;
	/**
	 * Deserializes the data on the provided stream and reconstitutes the graph of
	 * objects.
	 * @param serializationStream The stream that contains the data to deserialize.
	 * @return The top object of the deserialized graph.
	 */
	function Deserialize(serializationStream:cs.system.io.Stream):Dynamic;
	/**
	 * Serializes an object, or graph of objects with the given root to the provided
	 * stream.
	 * @param serializationStream The stream where the formatter puts the serialized
	 * data. This stream can reference a variety of backing stores (such as files,
	 * network, memory, and so on).
	 * @param graph The object, or root of the object graph, to serialize. All child
	 * objects of this root object are automatically serialized.
	 */
	function Serialize(serializationStream:cs.system.io.Stream, graph:Dynamic):Void;
}
