package cs.system.componentmodel;

/**
 * Represents the method that will handle the MethodName event of an asynchronous
 * operation.
 * @param sender The source of the event.
 * @param e An  that contains the event data.
 */
@:native("System.ComponentModel.AsyncCompletedEventHandler")
extern class AsyncCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.componentmodel.AsyncCompletedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.componentmodel.AsyncCompletedEventArgs):Void;
}
