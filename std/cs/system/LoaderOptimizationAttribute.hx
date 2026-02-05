package cs.system;

/** Used to set the default loader optimization policy for the main method of an executable application. */
@:native("System.LoaderOptimizationAttribute")
extern class LoaderOptimizationAttribute extends cs.system.Attribute {
	/**
	 * Gets the current  value for this instance.
	 * @return A  constant.
	 */
	var Value(default, never):cs.system.LoaderOptimization;
	@:overload(function(value:cs.UInt8):Void {})
	function new(value:cs.system.LoaderOptimization):Void;
}
