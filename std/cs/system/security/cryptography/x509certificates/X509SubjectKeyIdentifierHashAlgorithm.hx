package cs.system.security.cryptography.x509certificates;

/** Defines the type of hash algorithm to use with the  class. */
@:native("System.Security.Cryptography.X509Certificates.X509SubjectKeyIdentifierHashAlgorithm")
extern enum abstract X509SubjectKeyIdentifierHashAlgorithm(Int) {
	var CapiSha1 = 2;
	var Sha1 = 0;
	var ShortSha1 = 1;
}
