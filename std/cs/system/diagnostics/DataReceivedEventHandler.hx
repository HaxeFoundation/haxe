package cs.system.diagnostics;

/**
 * Represents the method that will handle the  event or  event of a .
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.Diagnostics.DataReceivedEventHandler")
extern class DataReceivedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.diagnostics.DataReceivedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.diagnostics.DataReceivedEventArgs):Void;
}
