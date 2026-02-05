package cs.system.net.networkinformation;

/** The scope level for an IPv6 address. */
@:native("System.Net.NetworkInformation.ScopeLevel")
extern enum abstract ScopeLevel(Int) {
	var Admin = 4;
	var Global = 14;
	var Interface = 1;
	var Link = 2;
	var None = 0;
	var Organization = 8;
	var Site = 5;
	var Subnet = 3;
}
