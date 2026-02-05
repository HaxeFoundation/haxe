package cs.system.net.networkinformation;

/** Specifies how an IP address host suffix was located. */
@:native("System.Net.NetworkInformation.SuffixOrigin")
extern enum SuffixOrigin {
	LinkLayerAddress;
	Manual;
	OriginDhcp;
	Other;
	Random;
	WellKnown;
}
