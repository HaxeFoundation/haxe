package cs.system.runtime.compilerservices;

/** Defines a general-purpose Tuple implementation that allows access to Tuple instance members without knowing the underlying Tuple type. */
@:native("System.Runtime.CompilerServices.ITuple")
extern interface ITuple {
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	/**
	 * Gets the number of elements in this  instance.
	 * @return The number of elements in this  instance.
	 */
	var Length(default, never):Int;
}
