package cs.system.net.networkinformation;

/**
 * Represents the method that will handle the  event of a  object.
 * @param sender The source of the  event.
 * @param e A  object that contains the event data.
 */
@:native("System.Net.NetworkInformation.PingCompletedEventHandler")
extern class PingCompletedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.net.networkinformation.PingCompletedEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.net.networkinformation.PingCompletedEventArgs):Void;
}
