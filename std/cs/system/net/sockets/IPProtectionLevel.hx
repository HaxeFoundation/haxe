package cs.system.net.sockets;

/** A value that enables restriction of an IPv6 socket to a specified scope, such as addresses with the same link local or site local prefix. */
@:native("System.Net.Sockets.IPProtectionLevel")
extern enum IPProtectionLevel {
	EdgeRestricted;
	Restricted;
	Unrestricted;
	Unspecified;
}
