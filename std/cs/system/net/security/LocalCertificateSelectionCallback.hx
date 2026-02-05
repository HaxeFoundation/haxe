package cs.system.net.security;

/**
 * Selects the local Secure Sockets Layer (SSL) certificate used for
 * authentication.
 * @param sender An object that contains state information for this validation.
 * @param targetHost The host server specified by the client.
 * @param localCertificates An  containing local certificates.
 * @param remoteCertificate The certificate used to authenticate the remote party.
 * @param acceptableIssuers A  array of certificate issuers acceptable to the
 * remote party.
 * @return An  used for establishing an SSL connection.
 */
@:native("System.Net.Security.LocalCertificateSelectionCallback")
extern class LocalCertificateSelectionCallback extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, targetHost:String, localCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, remoteCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, acceptableIssuers:cs.NativeArray<String>)->cs.system.security.cryptography.x509certificates.X509Certificate):Void;
	function Invoke(sender:Dynamic, targetHost:String, localCertificates:cs.system.security.cryptography.x509certificates.X509CertificateCollection, remoteCertificate:cs.system.security.cryptography.x509certificates.X509Certificate, acceptableIssuers:cs.NativeArray<String>):cs.system.security.cryptography.x509certificates.X509Certificate;
}
