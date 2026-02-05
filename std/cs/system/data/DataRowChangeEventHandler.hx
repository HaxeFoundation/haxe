package cs.system.data;

/**
 * Represents the method that will handle the , , , and  events of a .
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.Data.DataRowChangeEventHandler")
extern class DataRowChangeEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.DataRowChangeEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.DataRowChangeEventArgs):Void;
}
