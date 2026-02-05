package cs.system.componentmodel;

/**
 * Represents the method that will handle the  event of a  class.
 * @param sender The source of the event.
 * @param e A  that contains the event data.
 */
@:native("System.ComponentModel.RunWorkerCompletedEventHandler")
extern class RunWorkerCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.RunWorkerCompletedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.RunWorkerCompletedEventArgs):Void;
}
