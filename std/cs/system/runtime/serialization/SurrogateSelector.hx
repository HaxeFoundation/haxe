package cs.system.runtime.serialization;

/** Assists formatters in selection of the serialization surrogate to delegate the serialization or deserialization process to. */
@:native("System.Runtime.Serialization.SurrogateSelector")
extern class SurrogateSelector {
	function new():Void;
	/**
	 * Adds a surrogate to the list of checked surrogates.
	 * @param type The  for which the surrogate is required.
	 * @param context The context-specific data.
	 * @param surrogate The surrogate to call for this type.
	 */
	function AddSurrogate(type:cs.system.Type, context:cs.system.runtime.serialization.StreamingContext, surrogate:cs.system.runtime.serialization.ISerializationSurrogate):Void;
	/**
	 * Adds the specified  that can handle a particular object type to the list of
	 * surrogates.
	 * @param selector The surrogate selector to add.
	 */
	function ChainSelector(selector:cs.system.runtime.serialization.ISurrogateSelector):Void;
	/**
	 * Returns the next selector on the chain of selectors.
	 * @return The next  on the chain of selectors.
	 */
	function GetNextSelector():cs.system.runtime.serialization.ISurrogateSelector;
	/**
	 * Returns the surrogate for a particular type.
	 * @param type The  for which the surrogate is requested.
	 * @param context The streaming context.
	 * @param selector The surrogate to use.
	 * @return The surrogate for a particular type.
	 */
	function GetSurrogate(type:cs.system.Type, context:cs.system.runtime.serialization.StreamingContext, selector:cs.Ref<cs.system.runtime.serialization.ISurrogateSelector>):cs.system.runtime.serialization.ISerializationSurrogate;
	/**
	 * Removes the surrogate associated with a given type.
	 * @param type The  for which to remove the surrogate.
	 * @param context The  for the current surrogate.
	 */
	function RemoveSurrogate(type:cs.system.Type, context:cs.system.runtime.serialization.StreamingContext):Void;
}
