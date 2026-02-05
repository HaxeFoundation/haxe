package cs.system.net.networkinformation;

/** Specifies the Network Basic Input/Output System (NetBIOS) node type. */
@:native("System.Net.NetworkInformation.NetBiosNodeType")
extern enum abstract NetBiosNodeType(Int) {
	var Broadcast = 1;
	var Hybrid = 8;
	var Mixed = 4;
	var Peer2Peer = 2;
	var Unknown = 0;
}
