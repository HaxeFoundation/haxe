package cs.system.data;

/**
 * The delegate type for the event handlers of the  event.
 * @param sender The source of the event.
 * @param e The data for the event.
 */
@:native("System.Data.StatementCompletedEventHandler")
extern class StatementCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.data.StatementCompletedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.data.StatementCompletedEventArgs):Void;
}
