package cs.system.componentmodel;

/**
 * Represents a method that can handle events which may or may not require further
 * processing after the event handler has returned.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.HandledEventHandler")
extern class HandledEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.HandledEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.HandledEventArgs):Void;
}
