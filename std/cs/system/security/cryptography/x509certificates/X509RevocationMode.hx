package cs.system.security.cryptography.x509certificates;

/** Specifies the mode used to check for X509 certificate revocation. */
@:native("System.Security.Cryptography.X509Certificates.X509RevocationMode")
extern enum abstract X509RevocationMode(Int) {
	var NoCheck = 0;
	var Offline = 2;
	var Online = 1;
}
