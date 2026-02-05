package cs.system;

/** Represents a weak reference, which references an object while still allowing that object to be reclaimed by garbage collection. */
@:native("System.WeakReference")
extern class WeakReference {
	/**
	 * Gets an indication whether the object referenced by the current  object has been
	 * garbage collected.
	 * @return if the object referenced by the current  object has not been garbage
	 * collected and is still accessible; otherwise, .
	 */
	var IsAlive(default, never):Bool;
	/**
	 * Gets or sets the object (the target) referenced by the current  object.
	 * @return if the object referenced by the current  object has been garbage
	 * collected; otherwise, a reference to the object referenced by the current 
	 * object.
	 */
	var Target(default, default):Dynamic;
	/**
	 * Gets an indication whether the object referenced by the current  object is
	 * tracked after it is finalized.
	 * @return if the object the current  object refers to is tracked after
	 * finalization; or  if the object is only tracked until finalization.
	 */
	var TrackResurrection(default, never):Bool;
	@:overload(function(target:Dynamic):Void {})
	function new(target:Dynamic, trackResurrection:Bool):Void;
	/**
	 * Populates a  object with all the data needed to serialize the current  object.
	 * @param info An object that holds all the data needed to serialize or deserialize
	 * the current  object.
	 * @param context (Reserved) The location where serialized data is stored and
	 * retrieved.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
