package cs.system.security.cryptography.x509certificates;

/** Specifies the type of name the X509 certificate contains. */
@:native("System.Security.Cryptography.X509Certificates.X509NameType")
extern enum X509NameType {
	DnsFromAlternativeName;
	DnsName;
	EmailName;
	SimpleName;
	UpnName;
	UrlName;
}
