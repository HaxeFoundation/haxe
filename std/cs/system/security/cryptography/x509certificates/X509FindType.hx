package cs.system.security.cryptography.x509certificates;

/** Specifies the type of value the  method searches for. */
@:native("System.Security.Cryptography.X509Certificates.X509FindType")
extern enum abstract X509FindType(Int) {
	var FindByApplicationPolicy = 10;
	var FindByCertificatePolicy = 11;
	var FindByExtension = 12;
	var FindByIssuerDistinguishedName = 4;
	var FindByIssuerName = 3;
	var FindByKeyUsage = 13;
	var FindBySerialNumber = 5;
	var FindBySubjectDistinguishedName = 2;
	var FindBySubjectKeyIdentifier = 14;
	var FindBySubjectName = 1;
	var FindByTemplateName = 9;
	var FindByThumbprint = 0;
	var FindByTimeExpired = 8;
	var FindByTimeNotYetValid = 7;
	var FindByTimeValid = 6;
}
