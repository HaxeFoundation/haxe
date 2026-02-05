package cs.system.componentmodel;

/** Provides functionality required by sites. */
@:native("System.ComponentModel.ISite")
extern interface ISite extends cs.system.IServiceProvider {
	/**
	 * Gets the component associated with the  when implemented by a class.
	 * @return The  instance associated with the .
	 */
	var Component(default, never):cs.system.componentmodel.IComponent;
	/**
	 * Gets the  associated with the  when implemented by a class.
	 * @return The  instance associated with the .
	 */
	var Container(default, never):cs.system.componentmodel.IContainer;
	/**
	 * Determines whether the component is in design mode when implemented by a class.
	 * @return if the component is in design mode; otherwise, .
	 */
	var DesignMode(default, never):Bool;
	/**
	 * Gets or sets the name of the component associated with the  when implemented by
	 * a class.
	 * @return The name of the component associated with the ; or , if no name is
	 * assigned to the component.
	 */
	var Name(default, default):String;
}
