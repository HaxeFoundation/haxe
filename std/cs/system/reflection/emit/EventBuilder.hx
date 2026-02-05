package cs.system.reflection.emit;

/** Defines events for a class. */
@:native("System.Reflection.Emit.EventBuilder")
extern class EventBuilder {
	/**
	 * Adds one of the "other" methods associated with this event. "Other" methods are
	 * methods other than the "on" and "raise" methods associated with an event. This
	 * function can be called many times to add as many "other" methods.
	 * @param mdBuilder A  object that represents the other method.
	 */
	function AddOtherMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
	/**
	 * Sets the method used to subscribe to this event.
	 * @param mdBuilder A  object that represents the method used to subscribe to this
	 * event.
	 */
	function SetAddOnMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
	@:overload(function(customBuilder:cs.system.reflection.emit.CustomAttributeBuilder):Void {})
	/**
	 * Set a custom attribute using a specified custom attribute blob.
	 * @param con The constructor for the custom attribute.
	 * @param binaryAttribute A byte blob representing the attributes.
	 */
	function SetCustomAttribute(con:cs.system.reflection.ConstructorInfo, binaryAttribute:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Sets the method used to raise this event.
	 * @param mdBuilder A  object that represents the method used to raise this event.
	 */
	function SetRaiseMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
	/**
	 * Sets the method used to unsubscribe to this event.
	 * @param mdBuilder A  object that represents the method used to unsubscribe to
	 * this event.
	 */
	function SetRemoveOnMethod(mdBuilder:cs.system.reflection.emit.MethodBuilder):Void;
}
