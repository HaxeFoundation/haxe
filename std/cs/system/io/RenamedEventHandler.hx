package cs.system.io;

/**
 * Represents the method that will handle the  event of a  class.
 * @param sender The source of the event.
 * @param e The  that contains the event data.
 */
@:native("System.IO.RenamedEventHandler")
extern class RenamedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.io.RenamedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.io.RenamedEventArgs):Void;
}
