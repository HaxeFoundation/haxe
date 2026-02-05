package cs.system.componentmodel.design;

/** Provides data for the  event. This class cannot be inherited. */
@:native("System.ComponentModel.Design.ComponentChangingEventArgs")
extern class ComponentChangingEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the component that is about to be changed or the component that is the
	 * parent container of the member that is about to be changed.
	 * @return The component that is about to have a member changed.
	 */
	var Component(default, never):Dynamic;
	/**
	 * Gets the member that is about to be changed.
	 * @return A  indicating the member that is about to be changed, if known, or 
	 * otherwise.
	 */
	var Member(default, never):cs.system.componentmodel.MemberDescriptor;
	function new(component:Dynamic, member:cs.system.componentmodel.MemberDescriptor):Void;
}
