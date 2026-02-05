package cs.system;

/** Represents a weak reference, which references an object while still allowing that object to be reclaimed by garbage collection. */
@:native("System.WeakReference`1")
extern class WeakReference_1<T> {
	@:overload(function(target:T):Void {})
	function new(target:T, trackResurrection:Bool):Void;
	/**
	 * Populates a  object with all the data needed to serialize the current  object.
	 * @param info An object that holds all the data needed to serialize or deserialize
	 * the current  object.
	 * @param context (Reserved) The location where serialized data is stored and
	 * retrieved.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	function SetTarget(target:T):Void;
	function TryGetTarget(target:cs.Ref<T>):Bool;
}
