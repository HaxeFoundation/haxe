package cs.system.runtime.interopservices;

/** Permits late-bound registration of an event handler. */
@:native("System.Runtime.InteropServices.ComAwareEventInfo")
extern class ComAwareEventInfo extends cs.system.reflection.EventInfo {
	function new(type:cs.system.Type, eventName:String):Void;
	/**
	 * Attaches an event handler to a COM object.
	 * @param target The target object that the event delegate should bind to.
	 * @param handler The event delegate.
	 */
	function AddEventHandler(target:Dynamic, handler:cs.system.Delegate):Void;
	/**
	 * Gets the method that was used to add an event handler delegate to the event
	 * source.
	 * @param nonPublic to return non-public methods; otherwise, .
	 * @return The method that was used to add an event handler delegate to the event
	 * source.
	 */
	function GetAddMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * When overridden in a derived class, gets an array that contains all the custom
	 * attributes that are applied to this member.
	 * @param inherit to search this member's inheritance chain to find the attributes;
	 * otherwise, .
	 * @return An array that contains all the custom attributes, or an array that has
	 * no elements if no attributes were defined.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * When overridden in a derived class, returns the method that was called when the
	 * event was raised.
	 * @param nonPublic to return non-public methods; otherwise, .
	 * @return The object that was called when the event was raised.
	 */
	function GetRaiseMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	/**
	 * When overridden in a derived class, retrieves the  object for removing a method
	 * of the event.
	 * @param nonPublic to return non-public methods; otherwise, .
	 * @return The method that was used to remove an event handler delegate from the
	 * event source.
	 */
	function GetRemoveMethod(nonPublic:Bool):cs.system.reflection.MethodInfo;
	/**
	 * Indicates whether one or more instances of the specified attribute are applied
	 * to this member.
	 * @param attributeType The attribute type to search for.
	 * @param inherit to search this member's inheritance chain to find the attributes;
	 * otherwise, .
	 * @return if the specified attribute has been applied to this member; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	/**
	 * Detaches an event handler from a COM object.
	 * @param target The target object that the event delegate is bound to.
	 * @param handler The event delegate.
	 */
	function RemoveEventHandler(target:Dynamic, handler:cs.system.Delegate):Void;
}
