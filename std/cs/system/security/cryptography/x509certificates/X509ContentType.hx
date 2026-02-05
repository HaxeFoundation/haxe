package cs.system.security.cryptography.x509certificates;

/** Specifies the format of an X.509 certificate. */
@:native("System.Security.Cryptography.X509Certificates.X509ContentType")
extern enum abstract X509ContentType(Int) {
	var Authenticode = 6;
	var Cert = 1;
	var Pfx = 3;
	var Pkcs12 = 3;
	var Pkcs7 = 5;
	var SerializedCert = 2;
	var SerializedStore = 4;
	var Unknown = 0;
}
