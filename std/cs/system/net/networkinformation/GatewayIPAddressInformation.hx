package cs.system.net.networkinformation;

/** Represents the IP address of the network gateway. This class cannot be instantiated. */
@:native("System.Net.NetworkInformation.GatewayIPAddressInformation")
extern class GatewayIPAddressInformation {
	/**
	 * Gets the IP address of the gateway.
	 * @return An  object that contains the IP address of the gateway.
	 */
	var Address(default, never):cs.system.net.IPAddress;
}
