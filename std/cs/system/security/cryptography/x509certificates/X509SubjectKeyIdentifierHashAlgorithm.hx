package cs.system.security.cryptography.x509certificates;

/** Defines the type of hash algorithm to use with the  class. */
@:native("System.Security.Cryptography.X509Certificates.X509SubjectKeyIdentifierHashAlgorithm")
extern enum X509SubjectKeyIdentifierHashAlgorithm {
	CapiSha1;
	Sha1;
	ShortSha1;
}
