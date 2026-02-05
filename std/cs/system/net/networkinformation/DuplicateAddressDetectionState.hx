package cs.system.net.networkinformation;

/** Specifies the current state of an IP address. */
@:native("System.Net.NetworkInformation.DuplicateAddressDetectionState")
extern enum DuplicateAddressDetectionState {
	Deprecated;
	Duplicate;
	Invalid;
	Preferred;
	Tentative;
}
