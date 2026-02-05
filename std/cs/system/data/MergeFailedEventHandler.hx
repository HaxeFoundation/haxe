package cs.system.data;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e The data for the event.
 */
@:native("System.Data.MergeFailedEventHandler")
extern class MergeFailedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.MergeFailedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.MergeFailedEventArgs):Void;
}
