package cs.system.security.cryptography.x509certificates;

@:native("System.Security.Cryptography.X509Certificates.X509CertificateCollection.X509CertificateEnumerator")
extern class X509CertificateCollection_X509CertificateEnumerator {
	var Current(default, never):cs.system.security.cryptography.x509certificates.X509Certificate;
	function new(mappings:cs.system.security.cryptography.x509certificates.X509CertificateCollection):Void;
	function MoveNext():Bool;
	function Reset():Void;
}
