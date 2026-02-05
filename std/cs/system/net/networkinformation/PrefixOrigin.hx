package cs.system.net.networkinformation;

/** Specifies how an IP address network prefix was located. */
@:native("System.Net.NetworkInformation.PrefixOrigin")
extern enum abstract PrefixOrigin(Int) {
	var Dhcp = 3;
	var Manual = 1;
	var Other = 0;
	var RouterAdvertisement = 4;
	var WellKnown = 2;
}
