package cs.system.net.networkinformation;

/** Provides information about a network interface's multicast address. */
@:native("System.Net.NetworkInformation.MulticastIPAddressInformation")
extern class MulticastIPAddressInformation extends cs.system.net.networkinformation.IPAddressInformation {
	/**
	 * Gets the number of seconds remaining during which this address is the preferred
	 * address.
	 * @return An  value that specifies the number of seconds left for this address to
	 * remain preferred.
	 */
	var AddressPreferredLifetime(default, never):haxe.Int64;
	/**
	 * Gets the number of seconds remaining during which this address is valid.
	 * @return An  value that specifies the number of seconds left for this address to
	 * remain assigned.
	 */
	var AddressValidLifetime(default, never):haxe.Int64;
	/**
	 * Specifies the amount of time remaining on the Dynamic Host Configuration
	 * Protocol (DHCP) lease for this IP address.
	 * @return An  value that contains the number of seconds remaining before the
	 * computer must release the  instance.
	 */
	var DhcpLeaseLifetime(default, never):haxe.Int64;
	/**
	 * Gets a value that indicates the state of the duplicate address detection
	 * algorithm.
	 * @return One of the  values that indicates the progress of the algorithm in
	 * determining the uniqueness of this IP address.
	 */
	var DuplicateAddressDetectionState(default, never):cs.system.net.networkinformation.DuplicateAddressDetectionState;
	/**
	 * Gets a value that identifies the source of a Multicast Internet Protocol (IP)
	 * address prefix.
	 * @return One of the  values that identifies how the prefix information was
	 * obtained.
	 */
	var PrefixOrigin(default, never):cs.system.net.networkinformation.PrefixOrigin;
	/**
	 * Gets a value that identifies the source of a Multicast Internet Protocol (IP)
	 * address suffix.
	 * @return One of the  values that identifies how the suffix information was
	 * obtained.
	 */
	var SuffixOrigin(default, never):cs.system.net.networkinformation.SuffixOrigin;
}
