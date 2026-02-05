package cs.system;

/**
 * Represents the method that will handle an event that has no event data.
 * @param sender The source of the event.
 * @param e An object that contains no event data.
 */
@:native("System.EventHandler`1")
extern class EventHandler_1<TEventArgs> extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:TEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:TEventArgs):Void;
}
