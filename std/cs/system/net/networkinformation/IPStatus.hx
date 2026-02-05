package cs.system.net.networkinformation;

/** Reports the status of sending an Internet Control Message Protocol (ICMP) echo message to a computer. */
@:native("System.Net.NetworkInformation.IPStatus")
extern enum abstract IPStatus(Int) {
	var BadDestination = 11018;
	var BadHeader = 11042;
	var BadOption = 11007;
	var BadRoute = 11012;
	var DestinationHostUnreachable = 11003;
	var DestinationNetworkUnreachable = 11002;
	var DestinationPortUnreachable = 11005;
	var DestinationProhibited = 11004;
	var DestinationProtocolUnreachable = 11004;
	var DestinationScopeMismatch = 11045;
	var DestinationUnreachable = 11040;
	var HardwareError = 11008;
	var IcmpError = 11044;
	var NoResources = 11006;
	var PacketTooBig = 11009;
	var ParameterProblem = 11015;
	var SourceQuench = 11016;
	var Success = 0;
	var TimedOut = 11010;
	var TimeExceeded = 11041;
	var TtlExpired = 11013;
	var TtlReassemblyTimeExceeded = 11014;
	var Unknown = -1;
	var UnrecognizedNextHeader = 11043;
}
