package cs.system.net.security;

@:native("System.Net.Security.SslServerAuthenticationOptions")
extern class SslServerAuthenticationOptions {
	var AllowRenegotiation(default, default):Bool;
	var ApplicationProtocols(default, default):cs.system.collections.generic.List<cs.system.net.security.SslApplicationProtocol>;
	var CertificateRevocationCheckMode(default, default):cs.system.security.cryptography.x509certificates.X509RevocationMode;
	var ClientCertificateRequired(default, default):Bool;
	var EnabledSslProtocols(default, default):cs.system.security.authentication.SslProtocols;
	var EncryptionPolicy(default, default):cs.system.net.security.EncryptionPolicy;
	var RemoteCertificateValidationCallback(default, default):cs.system.net.security.RemoteCertificateValidationCallback;
	var ServerCertificate(default, default):cs.system.security.cryptography.x509certificates.X509Certificate;
	var ServerCertificateSelectionCallback(default, default):cs.system.net.security.ServerCertificateSelectionCallback;
	function new():Void;
}
