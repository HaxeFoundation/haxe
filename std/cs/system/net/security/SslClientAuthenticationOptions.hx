package cs.system.net.security;

@:native("System.Net.Security.SslClientAuthenticationOptions")
extern class SslClientAuthenticationOptions {
	var AllowRenegotiation(default, default):Bool;
	var ApplicationProtocols(default, default):cs.system.collections.generic.List<cs.system.net.security.SslApplicationProtocol>;
	var CertificateRevocationCheckMode(default, default):cs.system.security.cryptography.x509certificates.X509RevocationMode;
	var ClientCertificates(default, default):cs.system.security.cryptography.x509certificates.X509CertificateCollection;
	var EnabledSslProtocols(default, default):cs.system.security.authentication.SslProtocols;
	var EncryptionPolicy(default, default):cs.system.net.security.EncryptionPolicy;
	var LocalCertificateSelectionCallback(default, default):cs.system.net.security.LocalCertificateSelectionCallback;
	var RemoteCertificateValidationCallback(default, default):cs.system.net.security.RemoteCertificateValidationCallback;
	var TargetHost(default, default):String;
	function new():Void;
}
