package cs.system.net.networkinformation;

/** Specifies the Network Basic Input/Output System (NetBIOS) node type. */
@:native("System.Net.NetworkInformation.NetBiosNodeType")
extern enum NetBiosNodeType {
	Broadcast;
	Hybrid;
	Mixed;
	Peer2Peer;
	Unknown;
}
