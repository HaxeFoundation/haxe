package cs.system.net.security;

/**
 * Verifies the remote Secure Sockets Layer (SSL) certificate used for
 * authentication.
 * @param sender An object that contains state information for this validation.
 * @param certificate The certificate used to authenticate the remote party.
 * @param chain The chain of certificate authorities associated with the remote
 * certificate.
 * @param sslPolicyErrors One or more errors associated with the remote
 * certificate.
 * @return A  value that determines whether the specified certificate is accepted
 * for authentication.
 */
@:native("System.Net.Security.RemoteCertificateValidationCallback")
extern class RemoteCertificateValidationCallback extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, certificate:cs.system.security.cryptography.x509certificates.X509Certificate, chain:cs.system.security.cryptography.x509certificates.X509Chain, sslPolicyErrors:cs.system.net.security.SslPolicyErrors)->Bool):Void;
	function Invoke(sender:Dynamic, certificate:cs.system.security.cryptography.x509certificates.X509Certificate, chain:cs.system.security.cryptography.x509certificates.X509Chain, sslPolicyErrors:cs.system.net.security.SslPolicyErrors):Bool;
}
