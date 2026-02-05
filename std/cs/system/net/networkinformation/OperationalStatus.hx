package cs.system.net.networkinformation;

/** Specifies the operational state of a network interface. */
@:native("System.Net.NetworkInformation.OperationalStatus")
extern enum OperationalStatus {
	Dormant;
	Down;
	LowerLayerDown;
	NotPresent;
	Testing;
	Unknown;
	Up;
}
