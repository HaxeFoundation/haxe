package cs.system.net.networkinformation;

/** Specifies the Internet Protocol versions that are supported by a network interface. */
@:native("System.Net.NetworkInformation.NetworkInterfaceComponent")
extern enum abstract NetworkInterfaceComponent(Int) {
	var IPv4 = 0;
	var IPv6 = 1;
}
