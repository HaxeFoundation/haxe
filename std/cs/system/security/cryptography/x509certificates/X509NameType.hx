package cs.system.security.cryptography.x509certificates;

/** Specifies the type of name the X509 certificate contains. */
@:native("System.Security.Cryptography.X509Certificates.X509NameType")
extern enum abstract X509NameType(Int) {
	var DnsFromAlternativeName = 4;
	var DnsName = 3;
	var EmailName = 1;
	var SimpleName = 0;
	var UpnName = 2;
	var UrlName = 5;
}
