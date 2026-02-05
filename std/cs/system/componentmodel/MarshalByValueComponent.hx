package cs.system.componentmodel;

/** Implements  and provides the base implementation for remotable components that are marshaled by value (a copy of the serialized object is passed). */
@:native("System.ComponentModel.MarshalByValueComponent")
extern class MarshalByValueComponent {
	/**
	 * Gets the container for the component.
	 * @return An object implementing the  interface that represents the component's
	 * container, or  if the component does not have a site.
	 */
	var Container(default, never):cs.system.componentmodel.IContainer;
	/**
	 * Gets a value indicating whether the component is currently in design mode.
	 * @return if the component is in design mode; otherwise, .
	 */
	var DesignMode(default, never):Bool;
	/**
	 * Gets the list of event handlers that are attached to this component.
	 * @return An  that provides the delegates for this component.
	 */
	var Events(default, never):cs.system.componentmodel.EventHandlerList;
	/**
	 * Gets or sets the site of the component.
	 * @return An object implementing the  interface that represents the site of the
	 * component.
	 */
	var Site(default, default):cs.system.componentmodel.ISite;
	function new():Void;
	/** Releases all resources used by the . */
	function Dispose():Void;
	/**
	 * Gets the implementer of the .
	 * @param service A  that represents the type of service you want.
	 * @return An  that represents the implementer of the .
	 */
	function GetService(service:cs.system.Type):Dynamic;
	/**
	 * Returns a  containing the name of the , if any. This method should not be
	 * overridden.
	 * @return A  containing the name of the , if any. if the  is unnamed.
	 */
	function ToString():String;
}
