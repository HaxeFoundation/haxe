package cs.system.data;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e The  that contains the event data.
 */
@:native("System.Data.FillErrorEventHandler")
extern class FillErrorEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.FillErrorEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.FillErrorEventArgs):Void;
}
