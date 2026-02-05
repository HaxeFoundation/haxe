package cs.system.timers;

/**
 * Represents the method that will handle the  event of a .
 * @param sender The source of the event.
 * @param e An  object that contains the event data.
 */
@:native("System.Timers.ElapsedEventHandler")
extern class ElapsedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.timers.ElapsedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.timers.ElapsedEventArgs):Void;
}
