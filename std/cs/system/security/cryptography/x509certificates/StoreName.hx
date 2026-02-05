package cs.system.security.cryptography.x509certificates;

/** Specifies the name of the X.509 certificate store to open. */
@:native("System.Security.Cryptography.X509Certificates.StoreName")
extern enum StoreName {
	AddressBook;
	AuthRoot;
	CertificateAuthority;
	Disallowed;
	My;
	Root;
	TrustedPeople;
	TrustedPublisher;
}
