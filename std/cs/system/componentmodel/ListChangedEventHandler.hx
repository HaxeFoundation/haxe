package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event of the  class.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.ListChangedEventHandler")
extern class ListChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.ListChangedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.ListChangedEventArgs):Void;
}
