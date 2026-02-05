package cs.system.data;

/**
 * Represents the method that handles the  method.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.Data.DataTableNewRowEventHandler")
extern class DataTableNewRowEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.DataTableNewRowEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.DataTableNewRowEventArgs):Void;
}
