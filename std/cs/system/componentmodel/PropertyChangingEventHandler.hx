package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event of an  interface.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.PropertyChangingEventHandler")
extern class PropertyChangingEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.PropertyChangingEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.PropertyChangingEventArgs):Void;
}
