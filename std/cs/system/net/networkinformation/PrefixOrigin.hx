package cs.system.net.networkinformation;

/** Specifies how an IP address network prefix was located. */
@:native("System.Net.NetworkInformation.PrefixOrigin")
extern enum PrefixOrigin {
	Dhcp;
	Manual;
	Other;
	RouterAdvertisement;
	WellKnown;
}
