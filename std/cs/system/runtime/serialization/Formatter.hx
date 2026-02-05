package cs.system.runtime.serialization;

/** Provides base functionality for the common language runtime serialization formatters. */
@:native("System.Runtime.Serialization.Formatter")
extern class Formatter {
	/**
	 * When overridden in a derived class, gets or sets the  used with the current
	 * formatter.
	 * @return The  used with the current formatter.
	 */
	var Binder(default, default):cs.system.runtime.serialization.SerializationBinder;
	/**
	 * When overridden in a derived class, gets or sets the  used for the current
	 * serialization.
	 * @return The  used for the current serialization.
	 */
	var Context(default, default):cs.system.runtime.serialization.StreamingContext;
	/**
	 * When overridden in a derived class, gets or sets the  used with the current
	 * formatter.
	 * @return The  used with the current formatter.
	 */
	var SurrogateSelector(default, default):cs.system.runtime.serialization.ISurrogateSelector;
	/**
	 * When overridden in a derived class, deserializes the stream attached to the
	 * formatter when it was created, creating a graph of objects identical to the
	 * graph originally serialized into that stream.
	 * @param serializationStream The stream to deserialize.
	 * @return The top object of the deserialized graph of objects.
	 */
	function Deserialize(serializationStream:cs.system.io.Stream):Dynamic;
	/**
	 * When overridden in a derived class, serializes the graph of objects with the
	 * specified root to the stream already attached to the formatter.
	 * @param serializationStream The stream to which the objects are serialized.
	 * @param graph The object at the root of the graph to serialize.
	 */
	function Serialize(serializationStream:cs.system.io.Stream, graph:Dynamic):Void;
}
