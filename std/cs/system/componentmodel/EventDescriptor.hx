package cs.system.componentmodel;

/** Provides information about an event. */
@:native("System.ComponentModel.EventDescriptor")
extern class EventDescriptor extends cs.system.componentmodel.MemberDescriptor {
	/**
	 * When overridden in a derived class, gets the type of component this event is
	 * bound to.
	 * @return A  that represents the type of component the event is bound to.
	 */
	var ComponentType(default, never):cs.system.Type;
	/**
	 * When overridden in a derived class, gets the type of delegate for the event.
	 * @return A  that represents the type of delegate for the event.
	 */
	var EventType(default, never):cs.system.Type;
	/**
	 * When overridden in a derived class, gets a value indicating whether the event
	 * delegate is a multicast delegate.
	 * @return if the event delegate is multicast; otherwise, .
	 */
	var IsMulticast(default, never):Bool;
	/**
	 * When overridden in a derived class, binds the event to the component.
	 * @param component A component that provides events to the delegate.
	 * @param value A delegate that represents the method that handles the event.
	 */
	function AddEventHandler(component:Dynamic, value:cs.system.Delegate):Void;
	/**
	 * When overridden in a derived class, unbinds the delegate from the component so
	 * that the delegate will no longer receive events from the component.
	 * @param component The component that the delegate is bound to.
	 * @param value The delegate to unbind from the component.
	 */
	function RemoveEventHandler(component:Dynamic, value:cs.system.Delegate):Void;
}
