package cs.system.componentmodel.design;

/** Provides data for the  event. This class cannot be inherited. */
@:native("System.ComponentModel.Design.ComponentChangedEventArgs")
extern class ComponentChangedEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the component that was modified.
	 * @return An  that represents the component that was modified.
	 */
	var Component(default, never):Dynamic;
	/**
	 * Gets the member that has been changed.
	 * @return A  that indicates the member that has been changed.
	 */
	var Member(default, never):cs.system.componentmodel.MemberDescriptor;
	/**
	 * Gets the new value of the changed member.
	 * @return The new value of the changed member. This property can be .
	 */
	var NewValue(default, never):Dynamic;
	/**
	 * Gets the old value of the changed member.
	 * @return The old value of the changed member. This property can be .
	 */
	var OldValue(default, never):Dynamic;
	function new(component:Dynamic, member:cs.system.componentmodel.MemberDescriptor, oldValue:Dynamic, newValue:Dynamic):Void;
}
