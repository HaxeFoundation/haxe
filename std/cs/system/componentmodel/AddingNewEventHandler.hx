package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event, typically a data container or data-bound
 * collection.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.AddingNewEventHandler")
extern class AddingNewEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.AddingNewEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.AddingNewEventArgs):Void;
}
