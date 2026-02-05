package cs.system.data;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e The  that contains the event data.
 */
@:native("System.Data.StateChangeEventHandler")
extern class StateChangeEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.StateChangeEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.StateChangeEventArgs):Void;
}
