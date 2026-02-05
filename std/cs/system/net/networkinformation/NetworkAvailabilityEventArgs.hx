package cs.system.net.networkinformation;

/** Provides data for the  event. */
@:native("System.Net.NetworkInformation.NetworkAvailabilityEventArgs")
extern class NetworkAvailabilityEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the current status of the network connection.
	 * @return if the network is available; otherwise, .
	 */
	var IsAvailable(default, never):Bool;
}
