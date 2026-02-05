package cs.system.security.cryptography.x509certificates;

/** Specifies the location of the X.509 certificate store. */
@:native("System.Security.Cryptography.X509Certificates.StoreLocation")
extern enum abstract StoreLocation(Int) {
	var CurrentUser = 1;
	var LocalMachine = 2;
}
