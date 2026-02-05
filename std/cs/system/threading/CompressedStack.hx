package cs.system.threading;

/** Provides methods for setting and capturing the compressed stack on the current thread. This class cannot be inherited. */
@:native("System.Threading.CompressedStack")
extern class CompressedStack {
	/**
	 * Captures the compressed stack from the current thread.
	 * @return A  object.
	 */
	static function Capture():cs.system.threading.CompressedStack;
	/**
	 * Gets the compressed stack for the current thread.
	 * @return A  for the current thread.
	 */
	static function GetCompressedStack():cs.system.threading.CompressedStack;
	/**
	 * Runs a method in the specified compressed stack on the current thread.
	 * @param compressedStack The  to set.
	 * @param callback A  that represents the method to be run in the specified
	 * security context.
	 * @param state The object to be passed to the callback method.
	 */
	static function Run(compressedStack:cs.system.threading.CompressedStack, callback:cs.system.threading.ContextCallback, state:Dynamic):Void;
	/**
	 * Creates a copy of the current compressed stack.
	 * @return A  object representing the current compressed stack.
	 */
	function CreateCopy():cs.system.threading.CompressedStack;
	/**
	 * Sets the  object with the logical context information needed to recreate an
	 * instance of this execution context.
	 * @param info The  object to be populated with serialization information.
	 * @param context The  structure representing the destination context of the
	 * serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
