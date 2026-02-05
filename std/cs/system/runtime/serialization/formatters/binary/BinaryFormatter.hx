package cs.system.runtime.serialization.formatters.binary;

/** Serializes and deserializes an object, or an entire graph of connected objects, in binary format. */
@:native("System.Runtime.Serialization.Formatters.Binary.BinaryFormatter")
extern class BinaryFormatter {
	/**
	 * Gets or sets the behavior of the deserializer with regards to finding and
	 * loading assemblies.
	 * @return One of the  values that specifies the deserializer behavior.
	 */
	var AssemblyFormat(default, default):cs.system.runtime.serialization.formatters.FormatterAssemblyStyle;
	/**
	 * Gets or sets an object of type  that controls the binding of a serialized object
	 * to a type.
	 * @return The serialization binder to use with this formatter.
	 */
	var Binder(default, default):cs.system.runtime.serialization.SerializationBinder;
	/**
	 * Gets or sets the  for this formatter.
	 * @return The streaming context to use with this formatter.
	 */
	var Context(default, default):cs.system.runtime.serialization.StreamingContext;
	/**
	 * Gets or sets the  of automatic deserialization the  performs.
	 * @return The  that represents the current automatic deserialization level.
	 */
	var FilterLevel(default, default):cs.system.runtime.serialization.formatters.TypeFilterLevel;
	/**
	 * Gets or sets a  that controls type substitution during serialization and
	 * deserialization.
	 * @return The surrogate selector to use with this formatter.
	 */
	var SurrogateSelector(default, default):cs.system.runtime.serialization.ISurrogateSelector;
	/**
	 * Gets or sets the format in which type descriptions are laid out in the
	 * serialized stream.
	 * @return The style of type layouts to use.
	 */
	var TypeFormat(default, default):cs.system.runtime.serialization.formatters.FormatterTypeStyle;
	@:overload(function():Void {})
	function new(selector:cs.system.runtime.serialization.ISurrogateSelector, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Deserializes the specified stream into an object graph.
	 * @param serializationStream The stream from which to deserialize the object
	 * graph.
	 * @return The top (root) of the object graph.
	 */
	function Deserialize(serializationStream:cs.system.io.Stream):Dynamic;
	/**
	 * Serializes the object, or graph of objects with the specified top (root), to the
	 * given stream.
	 * @param serializationStream The stream to which the graph is to be serialized.
	 * @param graph The object at the root of the graph to serialize.
	 */
	function Serialize(serializationStream:cs.system.io.Stream, graph:Dynamic):Void;
}
