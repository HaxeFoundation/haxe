package cs.system.net.sockets;

/** Specifies the type of protocol that an instance of the  class can use. */
@:native("System.Net.Sockets.ProtocolFamily")
extern enum abstract ProtocolFamily(Int) {
	var AppleTalk = 16;
	var Atm = 22;
	var Banyan = 21;
	var Ccitt = 10;
	var Chaos = 5;
	var Cluster = 24;
	var DataKit = 9;
	var DataLink = 13;
	var DecNet = 12;
	var Ecma = 8;
	var FireFox = 19;
	var HyperChannel = 15;
	var Ieee12844 = 25;
	var ImpLink = 3;
	var InterNetwork = 2;
	var InterNetworkV6 = 23;
	var Ipx = 6;
	var Irda = 26;
	var Iso = 7;
	var Lat = 14;
	var Max = 29;
	var NetBios = 17;
	var NetworkDesigners = 28;
	var NS = 6;
	var Osi = 7;
	var Pup = 4;
	var Sna = 11;
	var Unix = 1;
	var Unknown = -1;
	var Unspecified = 0;
	var VoiceView = 18;
}
