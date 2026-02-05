package cs.system.net.networkinformation;

/** Reports the status of sending an Internet Control Message Protocol (ICMP) echo message to a computer. */
@:native("System.Net.NetworkInformation.IPStatus")
extern enum IPStatus {
	BadDestination;
	BadHeader;
	BadOption;
	BadRoute;
	DestinationHostUnreachable;
	DestinationNetworkUnreachable;
	DestinationPortUnreachable;
	DestinationProhibited;
	DestinationProtocolUnreachable;
	DestinationScopeMismatch;
	DestinationUnreachable;
	HardwareError;
	IcmpError;
	NoResources;
	PacketTooBig;
	ParameterProblem;
	SourceQuench;
	Success;
	TimedOut;
	TimeExceeded;
	TtlExpired;
	TtlReassemblyTimeExceeded;
	Unknown;
	UnrecognizedNextHeader;
}
