package cs.system;

/** Defines host name types for the  method. */
@:native("System.UriHostNameType")
extern enum abstract UriHostNameType(Int) {
	var Basic = 1;
	var Dns = 2;
	var IPv4 = 3;
	var IPv6 = 4;
	var Unknown = 0;
}
