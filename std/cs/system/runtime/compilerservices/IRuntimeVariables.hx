package cs.system.runtime.compilerservices;

/** Represents the values of run-time variables. */
@:native("System.Runtime.CompilerServices.IRuntimeVariables")
extern interface IRuntimeVariables {
	/**
	 * Gets a count of the run-time variables.
	 * @return The number of run-time variables.
	 */
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Int, value:Dynamic):Void;
}
