package cs.system.data;

/**
 * Represents the method that will handle the  event.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.Data.DataColumnChangeEventHandler")
extern class DataColumnChangeEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.DataColumnChangeEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.DataColumnChangeEventArgs):Void;
}
