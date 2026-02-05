package cs.system.net.networkinformation;

/** Specifies types of network interfaces. */
@:native("System.Net.NetworkInformation.NetworkInterfaceType")
extern enum abstract NetworkInterfaceType(Int) {
	var AsymmetricDsl = 94;
	var Atm = 37;
	var BasicIsdn = 20;
	var Ethernet = 6;
	var Ethernet3Megabit = 26;
	var FastEthernetFx = 69;
	var FastEthernetT = 62;
	var Fddi = 15;
	var GenericModem = 48;
	var GigabitEthernet = 117;
	var HighPerformanceSerialBus = 144;
	var IPOverAtm = 114;
	var Isdn = 63;
	var Loopback = 24;
	var MultiRateSymmetricDsl = 143;
	var Ppp = 23;
	var PrimaryIsdn = 21;
	var RateAdaptDsl = 95;
	var Slip = 28;
	var SymmetricDsl = 96;
	var TokenRing = 9;
	var Tunnel = 131;
	var Unknown = 1;
	var VeryHighSpeedDsl = 97;
	var Wireless80211 = 71;
	var Wman = 237;
	var Wwanpp = 243;
	var Wwanpp2 = 244;
}
