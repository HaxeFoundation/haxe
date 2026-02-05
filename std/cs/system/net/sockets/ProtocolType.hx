package cs.system.net.sockets;

/** Specifies the protocols that the  class supports. */
@:native("System.Net.Sockets.ProtocolType")
extern enum abstract ProtocolType(Int) {
	var Ggp = 3;
	var Icmp = 1;
	var IcmpV6 = 58;
	var Idp = 22;
	var Igmp = 2;
	var IP = 0;
	var IPSecAuthenticationHeader = 51;
	var IPSecEncapsulatingSecurityPayload = 50;
	var IPv4 = 4;
	var IPv6 = 41;
	var IPv6DestinationOptions = 60;
	var IPv6FragmentHeader = 44;
	var IPv6HopByHopOptions = 0;
	var IPv6NoNextHeader = 59;
	var IPv6RoutingHeader = 43;
	var Ipx = 1000;
	var ND = 77;
	var Pup = 12;
	var Raw = 255;
	var Spx = 1256;
	var SpxII = 1257;
	var Tcp = 6;
	var Udp = 17;
	var Unknown = -1;
	var Unspecified = 0;
}
