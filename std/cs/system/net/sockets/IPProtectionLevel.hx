package cs.system.net.sockets;

/** A value that enables restriction of an IPv6 socket to a specified scope, such as addresses with the same link local or site local prefix. */
@:native("System.Net.Sockets.IPProtectionLevel")
extern enum abstract IPProtectionLevel(Int) {
	var EdgeRestricted = 20;
	var Restricted = 30;
	var Unrestricted = 10;
	var Unspecified = -1;
}
