package cs.system.net.networkinformation;

/** Specifies the current state of an IP address. */
@:native("System.Net.NetworkInformation.DuplicateAddressDetectionState")
extern enum abstract DuplicateAddressDetectionState(Int) {
	var Deprecated = 3;
	var Duplicate = 2;
	var Invalid = 0;
	var Preferred = 4;
	var Tentative = 1;
}
