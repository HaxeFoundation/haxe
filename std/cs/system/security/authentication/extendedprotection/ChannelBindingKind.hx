package cs.system.security.authentication.extendedprotection;

/** The  enumeration represents the kinds of channel bindings that can be queried from secure channels. */
@:native("System.Security.Authentication.ExtendedProtection.ChannelBindingKind")
extern enum abstract ChannelBindingKind(Int) {
	var Endpoint = 26;
	var Unique = 25;
	var Unknown = 0;
}
