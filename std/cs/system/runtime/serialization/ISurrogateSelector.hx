package cs.system.runtime.serialization;

/** Indicates a serialization surrogate selector class. */
@:native("System.Runtime.Serialization.ISurrogateSelector")
extern interface ISurrogateSelector {
	/**
	 * Specifies the next  for surrogates to examine if the current instance does not
	 * have a surrogate for the specified type and assembly in the specified context.
	 * @param selector The next surrogate selector to examine.
	 */
	function ChainSelector(selector:cs.system.runtime.serialization.ISurrogateSelector):Void;
	/**
	 * Returns the next surrogate selector in the chain.
	 * @return The next surrogate selector in the chain or .
	 */
	function GetNextSelector():cs.system.runtime.serialization.ISurrogateSelector;
	/**
	 * Finds the surrogate that represents the specified object's type, starting with
	 * the specified surrogate selector for the specified serialization context.
	 * @param type The  of object (class) that needs a surrogate.
	 * @param context The source or destination context for the current serialization.
	 * @param selector When this method returns, contains a  that holds a reference to
	 * the surrogate selector where the appropriate surrogate was found. This parameter
	 * is passed uninitialized.
	 * @return The appropriate surrogate for the given type in the given context.
	 */
	function GetSurrogate(type:cs.system.Type, context:cs.system.runtime.serialization.StreamingContext, selector:cs.Ref<cs.system.runtime.serialization.ISurrogateSelector>):cs.system.runtime.serialization.ISerializationSurrogate;
}
