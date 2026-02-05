package cs.system.net.security;

/** Indicates the security services requested for an authenticated stream. */
@:native("System.Net.Security.ProtectionLevel")
extern enum abstract ProtectionLevel(Int) {
	var EncryptAndSign = 2;
	var None = 0;
	var Sign = 1;
}
