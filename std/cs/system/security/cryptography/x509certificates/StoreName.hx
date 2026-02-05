package cs.system.security.cryptography.x509certificates;

/** Specifies the name of the X.509 certificate store to open. */
@:native("System.Security.Cryptography.X509Certificates.StoreName")
extern enum abstract StoreName(Int) {
	var AddressBook = 1;
	var AuthRoot = 2;
	var CertificateAuthority = 3;
	var Disallowed = 4;
	var My = 5;
	var Root = 6;
	var TrustedPeople = 7;
	var TrustedPublisher = 8;
}
