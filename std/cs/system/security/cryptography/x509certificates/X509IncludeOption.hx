package cs.system.security.cryptography.x509certificates;

/** Specifies how much of the X.509 certificate chain should be included in the X.509 data. */
@:native("System.Security.Cryptography.X509Certificates.X509IncludeOption")
extern enum abstract X509IncludeOption(Int) {
	var EndCertOnly = 2;
	var ExcludeRoot = 1;
	var None = 0;
	var WholeChain = 3;
}
