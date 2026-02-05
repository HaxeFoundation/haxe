package cs.system.net.security;

/** Indicates the security services requested for an authenticated stream. */
@:native("System.Net.Security.ProtectionLevel")
extern enum ProtectionLevel {
	EncryptAndSign;
	None;
	Sign;
}
