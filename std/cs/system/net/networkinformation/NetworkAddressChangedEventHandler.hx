package cs.system.net.networkinformation;

/**
 * References one or more methods to be called when the address of a network
 * interface changes.
 * @param sender The source of the event.
 * @param e An  object that contains data about the event.
 */
@:native("System.Net.NetworkInformation.NetworkAddressChangedEventHandler")
extern class NetworkAddressChangedEventHandler extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, e:cs.system.EventArgs)->Void):Void;
	function Invoke(sender:Dynamic, e:cs.system.EventArgs):Void;
}
