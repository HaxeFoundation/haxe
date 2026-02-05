package cs.system.reflection;

/** Discovers the attributes of an event and provides access to event metadata. */
@:native("System.Reflection.EventInfo")
extern class EventInfo extends cs.system.reflection.MemberInfo {
	/**
	 * Gets the  object for the  method of the event, including non-public methods.
	 * @return The  object for the  method.
	 */
	var AddMethod(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the attributes for this event.
	 * @return The read-only attributes for this event.
	 */
	var Attributes(default, never):cs.system.reflection.EventAttributes;
	/**
	 * Gets the  object of the underlying event-handler delegate associated with this
	 * event.
	 * @return A read-only  object representing the delegate event handler.
	 */
	var EventHandlerType(default, never):cs.system.Type;
	/**
	 * Gets a value indicating whether the event is multicast.
	 * @return if the delegate is an instance of a multicast delegate; otherwise, .
	 */
	var IsMulticast(default, never):Bool;
	/**
	 * Gets a value indicating whether the  has a name with a special meaning.
	 * @return if this event has a special name; otherwise, .
	 */
	var IsSpecialName(default, never):Bool;
	/**
	 * Gets the method that is called when the event is raised, including non-public
	 * methods.
	 * @return The method that is called when the event is raised.
	 */
	var RaiseMethod(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the  object for removing a method of the event, including non-public
	 * methods.
	 * @return The  object for removing a method of the event.
	 */
	var RemoveMethod(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.EventInfo, right:cs.system.reflection.EventInfo):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first object to compare.
	 * @param right The second object to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.EventInfo, right:cs.system.reflection.EventInfo):Bool;
	/**
	 * Adds an event handler to an event source.
	 * @param target The event source.
	 * @param handler Encapsulates a method or methods to be invoked when the event is
	 * raised by the target.
	 */
	function AddEventHandler(target:Dynamic, handler:cs.system.Delegate):Void;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	@:overload(function():cs.system.reflection.MethodInfo {})
	/**
	 * Returns the method used to add an event handler delegate to the event source.
	 * @return A  object representing the method used to add an event handler delegate
	 * to the event source.
	 */
	function GetAddMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function():cs.NativeArray<cs.system.reflection.MethodInfo> {})
	/**
	 * Returns the public methods that have been associated with an event in metadata
	 * using the  directive.
	 * @return An array representing the public methods that have been associated with
	 * the event in metadata by using the  directive. If there are no such public
	 * methods, an empty array is returned.
	 */
	function GetOtherMethods(nonPublic:Bool):cs.NativeArray<cs.system.reflection.MethodInfo>;
	@:overload(function():cs.system.reflection.MethodInfo {})
	/**
	 * Returns the method that is called when the event is raised.
	 * @return The method that is called when the event is raised.
	 */
	function GetRaiseMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	@:overload(function():cs.system.reflection.MethodInfo {})
	/**
	 * Returns the method used to remove an event handler delegate from the event
	 * source.
	 * @return A  object representing the method used to remove an event handler
	 * delegate from the event source.
	 */
	function GetRemoveMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	/**
	 * Removes an event handler from an event source.
	 * @param target The event source.
	 * @param handler The delegate to be disassociated from the events raised by
	 * target.
	 */
	function RemoveEventHandler(target:Dynamic, handler:cs.system.Delegate):Void;
}
