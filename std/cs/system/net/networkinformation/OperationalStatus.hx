package cs.system.net.networkinformation;

/** Specifies the operational state of a network interface. */
@:native("System.Net.NetworkInformation.OperationalStatus")
extern enum abstract OperationalStatus(Int) {
	var Dormant = 5;
	var Down = 2;
	var LowerLayerDown = 7;
	var NotPresent = 6;
	var Testing = 3;
	var Unknown = 4;
	var Up = 1;
}
