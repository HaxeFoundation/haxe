package cs.system.componentmodel.design;

/**
 * Represents the method that will handle a  event.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.Design.ComponentRenameEventHandler")
extern class ComponentRenameEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.design.ComponentRenameEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.design.ComponentRenameEventArgs):Void;
}
