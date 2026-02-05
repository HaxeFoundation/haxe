package cs.system.net.sockets;

/** Defines configuration option names. */
@:native("System.Net.Sockets.SocketOptionName")
extern enum abstract SocketOptionName(Int) {
	var AcceptConnection = 2;
	var AddMembership = 12;
	var AddSourceMembership = 15;
	var BlockSource = 17;
	var Broadcast = 32;
	var BsdUrgent = 2;
	var ChecksumCoverage = 20;
	var Debug = 1;
	var DontFragment = 14;
	var DontLinger = -129;
	var DontRoute = 16;
	var DropMembership = 13;
	var DropSourceMembership = 16;
	var Error = 4103;
	var ExclusiveAddressUse = -5;
	var Expedited = 2;
	var HeaderIncluded = 2;
	var HopLimit = 21;
	var IPOptions = 1;
	var IPProtectionLevel = 23;
	var IpTimeToLive = 4;
	var IPv6Only = 27;
	var KeepAlive = 8;
	var Linger = 128;
	var MaxConnections = 2147483647;
	var MulticastInterface = 9;
	var MulticastLoopback = 11;
	var MulticastTimeToLive = 10;
	var NoChecksum = 1;
	var NoDelay = 1;
	var OutOfBandInline = 256;
	var PacketInformation = 19;
	var ReceiveBuffer = 4098;
	var ReceiveLowWater = 4100;
	var ReceiveTimeout = 4102;
	var ReuseAddress = 4;
	var ReuseUnicastPort = 12295;
	var SendBuffer = 4097;
	var SendLowWater = 4099;
	var SendTimeout = 4101;
	var Type = 4104;
	var TypeOfService = 3;
	var UnblockSource = 18;
	var UpdateAcceptContext = 28683;
	var UpdateConnectContext = 28688;
	var UseLoopback = 64;
}
