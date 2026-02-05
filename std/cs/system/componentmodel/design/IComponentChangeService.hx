package cs.system.componentmodel.design;

/** Provides an interface to add and remove the event handlers for events that add, change, remove or rename components, and provides methods to raise a  or  event. */
@:native("System.ComponentModel.Design.IComponentChangeService")
extern interface IComponentChangeService {
	/**
	 * Announces to the component change service that a particular component has
	 * changed.
	 * @param component The component that has changed.
	 * @param member The member that has changed. This is  if this change is not
	 * related to a single member.
	 * @param oldValue The old value of the member. This is valid only if the member is
	 * not .
	 * @param newValue The new value of the member. This is valid only if the member is
	 * not .
	 */
	function OnComponentChanged(component:Dynamic, member:cs.system.componentmodel.MemberDescriptor, oldValue:Dynamic, newValue:Dynamic):Void;
	/**
	 * Announces to the component change service that a particular component is
	 * changing.
	 * @param component The component that is about to change.
	 * @param member The member that is changing. This is  if this change is not
	 * related to a single member.
	 */
	function OnComponentChanging(component:Dynamic, member:cs.system.componentmodel.MemberDescriptor):Void;
}
