package cs.system.net.security;

/** The EncryptionPolicy to use. */
@:native("System.Net.Security.EncryptionPolicy")
extern enum EncryptionPolicy {
	AllowNoEncryption;
	NoEncryption;
	RequireEncryption;
}
