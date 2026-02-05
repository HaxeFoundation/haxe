package cs.system.security.cryptography.x509certificates;

/** Specifies which X509 certificates in the chain should be checked for revocation. */
@:native("System.Security.Cryptography.X509Certificates.X509RevocationFlag")
extern enum abstract X509RevocationFlag(Int) {
	var EndCertificateOnly = 0;
	var EntireChain = 1;
	var ExcludeRoot = 2;
}
