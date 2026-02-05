package cs.system.componentmodel.design;

/**
 * Represents the method that will handle a  event.
 * @param sender The source of the event.
 * @param e A  event that contains the event data.
 */
@:native("System.ComponentModel.Design.ComponentChangingEventHandler")
extern class ComponentChangingEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.ComponentChangingEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.ComponentChangingEventArgs):Void;
}
