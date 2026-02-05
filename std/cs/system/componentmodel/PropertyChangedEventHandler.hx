package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event raised when a property is
 * changed on a component.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.PropertyChangedEventHandler")
extern class PropertyChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.PropertyChangedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.PropertyChangedEventArgs):Void;
}
