package cs.system.componentmodel;

/**
 * Represents the method that handles the  event raised when adding elements to or
 * removing elements from a collection.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.CollectionChangeEventHandler")
extern class CollectionChangeEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.CollectionChangeEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.CollectionChangeEventArgs):Void;
}
