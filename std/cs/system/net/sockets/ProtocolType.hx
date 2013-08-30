
package cs.system.net.sockets;

@:native("System.Net.Sockets.ProtocolType")
extern enum ProtocolType {
	IP; // Internet Protocol. 
	IPv6HopByHopOptions; // IPv6 Hop by Hop Options header. 
	Icmp; // Internet Control Message Protocol. 
	Igmp; // Internet Group Management Protocol. 
	Ggp; // Gateway To Gateway Protocol. 
	IPv4; // Internet Protocol version 4. 
	Tcp; // Transmission Control Protocol. 
	Pup; // PARC Universal Packet Protocol. 
	Udp; // User Datagram Protocol. 
	Idp; // Internet Datagram Protocol. 
	IPv6; // Internet Protocol version 6 (IPv6).  
	IPv6RoutingHeader; // IPv6 Routing header. 
	IPv6FragmentHeader; // IPv6 Fragment header. 
	IPSecEncapsulatingSecurityPayload; // IPv6 Encapsulating Security Payload header. 
	IPSecAuthenticationHeader; // IPv6 Authentication header. For details, see RFC 2292 section 2.2.1, available at http://www.ietf.org. 
	IcmpV6; // Internet Control Message Protocol for IPv6. 
	IPv6NoNextHeader; // IPv6 No next header. 
	IPv6DestinationOptions; // IPv6 Destination Options header. 
	ND; // Net Disk Protocol (unofficial). 
	Raw; // Raw IP packet protocol. 
	Unspecified; // Unspecified protocol. 
	Ipx; // Internet Packet Exchange Protocol. 
	Spx; // Sequenced Packet Exchange protocol. 
	SpxII; // Sequenced Packet Exchange version 2 protocol. 
	Unknown; // Unknown protocol. 
}
