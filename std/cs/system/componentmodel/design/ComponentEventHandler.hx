package cs.system.componentmodel.design;

/**
 * Represents the method that will handle the , , , and  events raised for
 * component-level events.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.Design.ComponentEventHandler")
extern class ComponentEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.ComponentEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.ComponentEventArgs):Void;
}
