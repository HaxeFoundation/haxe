package cs.system;

/**
 * Represents the method that will handle the  event of a .
 * @param sender The source of the event.
 * @param e A  object that contains the event data.
 */
@:native("System.ConsoleCancelEventHandler")
extern class ConsoleCancelEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.ConsoleCancelEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.ConsoleCancelEventArgs):Void;
}
