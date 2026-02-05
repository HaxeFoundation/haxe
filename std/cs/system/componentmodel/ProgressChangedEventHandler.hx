package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event of the  class. This class
 * cannot be inherited.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.ProgressChangedEventHandler")
extern class ProgressChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.ProgressChangedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.ProgressChangedEventArgs):Void;
}
