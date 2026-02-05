package cs.system.net.sockets;

/** Specifies the protocols that the  class supports. */
@:native("System.Net.Sockets.ProtocolType")
extern enum ProtocolType {
	Ggp;
	Icmp;
	IcmpV6;
	Idp;
	Igmp;
	IP;
	IPSecAuthenticationHeader;
	IPSecEncapsulatingSecurityPayload;
	IPv4;
	IPv6;
	IPv6DestinationOptions;
	IPv6FragmentHeader;
	IPv6HopByHopOptions;
	IPv6NoNextHeader;
	IPv6RoutingHeader;
	Ipx;
	ND;
	Pup;
	Raw;
	Spx;
	SpxII;
	Tcp;
	Udp;
	Unknown;
	Unspecified;
}
