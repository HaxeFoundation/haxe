package cs.system.runtime.interopservices;

/** Tracks outstanding handles and forces a garbage collection when the specified threshold is reached. */
@:native("System.Runtime.InteropServices.HandleCollector")
extern class HandleCollector {
	/**
	 * Gets the number of handles collected.
	 * @return The number of handles collected.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that specifies the point at which collections should begin.
	 * @return A value that specifies the point at which collections should begin.
	 */
	var InitialThreshold(default, never):Int;
	/**
	 * Gets a value that specifies the point at which collections must occur.
	 * @return A value that specifies the point at which collections must occur.
	 */
	var MaximumThreshold(default, never):Int;
	/**
	 * Gets the name of a  object.
	 * @return This  property allows you to name collectors that track handle types
	 * separately.
	 */
	var Name(default, never):String;
	@:overload(function(name:String, initialThreshold:Int):Void {})
	function new(name:String, initialThreshold:Int, maximumThreshold:Int):Void;
	/** Increments the current handle count. */
	function Add():Void;
	/** Decrements the current handle count. */
	function Remove():Void;
}
