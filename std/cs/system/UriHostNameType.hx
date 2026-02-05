package cs.system;

/** Defines host name types for the  method. */
@:native("System.UriHostNameType")
extern enum UriHostNameType {
	Basic;
	Dns;
	IPv4;
	IPv6;
	Unknown;
}
