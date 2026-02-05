package cs.system.security.cryptography.x509certificates;

/** Specifies the type of value the  method searches for. */
@:native("System.Security.Cryptography.X509Certificates.X509FindType")
extern enum X509FindType {
	FindByApplicationPolicy;
	FindByCertificatePolicy;
	FindByExtension;
	FindByIssuerDistinguishedName;
	FindByIssuerName;
	FindByKeyUsage;
	FindBySerialNumber;
	FindBySubjectDistinguishedName;
	FindBySubjectKeyIdentifier;
	FindBySubjectName;
	FindByTemplateName;
	FindByThumbprint;
	FindByTimeExpired;
	FindByTimeNotYetValid;
	FindByTimeValid;
}
