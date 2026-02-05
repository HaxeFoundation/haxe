package cs.system.componentmodel;

/** Provides the base implementation for the  interface and enables object sharing between applications. */
@:native("System.ComponentModel.Component")
extern class Component extends cs.system.MarshalByRefObject {
	/**
	 * Gets a value indicating whether the component can raise an event.
	 * @return if the component can raise events; otherwise, . The default is .
	 */
	var CanRaiseEvents(default, never):Bool;
	/**
	 * Gets the  that contains the .
	 * @return The  that contains the , if any, or  if the  is not encapsulated in an .
	 */
	var Container(default, never):cs.system.componentmodel.IContainer;
	/**
	 * Gets a value that indicates whether the  is currently in design mode.
	 * @return if the  is in design mode; otherwise, .
	 */
	var DesignMode(default, never):Bool;
	/**
	 * Gets the list of event handlers that are attached to this .
	 * @return An  that provides the delegates for this component.
	 */
	var Events(default, never):cs.system.componentmodel.EventHandlerList;
	/**
	 * Gets or sets the  of the .
	 * @return The  associated with the , or  if the  is not encapsulated in an , the 
	 * does not have an  associated with it, or the  is removed from its .
	 */
	var Site(default, default):cs.system.componentmodel.ISite;
	function new():Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/**
	 * Returns a  containing the name of the , if any. This method should not be
	 * overridden.
	 * @return A  containing the name of the , if any, or  if the  is unnamed.
	 */
	function ToString():String;
}
