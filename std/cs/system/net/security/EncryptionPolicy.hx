package cs.system.net.security;

/** The EncryptionPolicy to use. */
@:native("System.Net.Security.EncryptionPolicy")
extern enum abstract EncryptionPolicy(Int) {
	var AllowNoEncryption = 1;
	var NoEncryption = 2;
	var RequireEncryption = 0;
}
