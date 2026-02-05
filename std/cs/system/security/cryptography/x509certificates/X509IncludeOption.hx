package cs.system.security.cryptography.x509certificates;

/** Specifies how much of the X.509 certificate chain should be included in the X.509 data. */
@:native("System.Security.Cryptography.X509Certificates.X509IncludeOption")
extern enum X509IncludeOption {
	EndCertOnly;
	ExcludeRoot;
	None;
	WholeChain;
}
