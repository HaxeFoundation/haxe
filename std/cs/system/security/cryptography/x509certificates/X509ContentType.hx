package cs.system.security.cryptography.x509certificates;

/** Specifies the format of an X.509 certificate. */
@:native("System.Security.Cryptography.X509Certificates.X509ContentType")
extern enum X509ContentType {
	Authenticode;
	Cert;
	Pfx;
	Pkcs12;
	Pkcs7;
	SerializedCert;
	SerializedStore;
	Unknown;
}
