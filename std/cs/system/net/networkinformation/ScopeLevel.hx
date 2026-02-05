package cs.system.net.networkinformation;

/** The scope level for an IPv6 address. */
@:native("System.Net.NetworkInformation.ScopeLevel")
extern enum ScopeLevel {
	Admin;
	Global;
	Interface;
	Link;
	None;
	Organization;
	Site;
	Subnet;
}
