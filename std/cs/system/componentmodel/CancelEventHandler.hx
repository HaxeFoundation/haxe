package cs.system.componentmodel;

/**
 * Represents the method that handles a cancelable event.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.CancelEventHandler")
extern class CancelEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.CancelEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.CancelEventArgs):Void;
}
