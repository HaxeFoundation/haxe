package cs.system.net.networkinformation;

/** Specifies how an IP address host suffix was located. */
@:native("System.Net.NetworkInformation.SuffixOrigin")
extern enum abstract SuffixOrigin(Int) {
	var LinkLayerAddress = 4;
	var Manual = 1;
	var OriginDhcp = 3;
	var Other = 0;
	var Random = 5;
	var WellKnown = 2;
}
