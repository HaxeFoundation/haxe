package cs.system;

/**
 * Represents the method that will handle an event that has no event data.
 * @param sender The source of the event.
 * @param e An object that contains no event data.
 */
@:native("System.EventHandler")
extern class EventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.EventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.EventArgs):Void;
}
