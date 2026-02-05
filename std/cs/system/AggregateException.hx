package cs.system;

/** Represents one or more errors that occur during application execution. */
@:native("System.AggregateException")
extern class AggregateException extends cs.system.Exception {
	/**
	 * Gets a read-only collection of the  instances that caused the current exception.
	 * @return A read-only collection of the  instances that caused the current
	 * exception.
	 */
	var InnerExceptions(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.Exception>;
	@:overload(function():Void {})
	@:overload(function(innerExceptions:cs.system.collections.generic.IEnumerable<cs.system.Exception>):Void {})
	@:overload(function(innerExceptions:cs.NativeArray<cs.system.Exception>):Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, innerExceptions:cs.system.collections.generic.IEnumerable<cs.system.Exception>):Void {})
	@:overload(function(message:String, innerException:cs.system.Exception):Void {})
	function new(message:String, innerExceptions:cs.NativeArray<cs.system.Exception>):Void;
	/**
	 * Flattens an  instances into a single, new instance.
	 * @return A new, flattened .
	 */
	function Flatten():cs.system.AggregateException;
	/**
	 * Returns the  that is the root cause of this exception.
	 * @return The  that is the root cause of this exception.
	 */
	function GetBaseException():cs.system.Exception;
	/**
	 * Initializes a new instance of the  class with serialized data.
	 * @param info The object that holds the serialized object data.
	 * @param context The contextual information about the source or destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Invokes a handler on each  contained by this .
	 * @param predicate The predicate to execute for each exception. The predicate
	 * accepts as an argument the  to be processed and returns a Boolean to indicate
	 * whether the exception was handled.
	 */
	function Handle(predicate:cs.system.Func_2<cs.system.Exception, Bool>):Void;
	/**
	 * Creates and returns a string representation of the current .
	 * @return A string representation of the current exception.
	 */
	function ToString():String;
}
