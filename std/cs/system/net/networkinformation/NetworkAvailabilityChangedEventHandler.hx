package cs.system.net.networkinformation;

/**
 * References one or more methods to be called when the availability of the network
 * changes.
 * @param sender The source of the event.
 * @param e An  object that contains data about the event.
 */
@:native("System.Net.NetworkInformation.NetworkAvailabilityChangedEventHandler")
extern class NetworkAvailabilityChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.net.networkinformation.NetworkAvailabilityEventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.net.networkinformation.NetworkAvailabilityEventArgs):Void;
}
