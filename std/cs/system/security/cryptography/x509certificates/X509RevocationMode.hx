package cs.system.security.cryptography.x509certificates;

/** Specifies the mode used to check for X509 certificate revocation. */
@:native("System.Security.Cryptography.X509Certificates.X509RevocationMode")
extern enum X509RevocationMode {
	NoCheck;
	Offline;
	Online;
}
