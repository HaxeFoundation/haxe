package cs.system.net.security;

/**
 * @param sender 
 * @param hostName 
 */
@:native("System.Net.Security.ServerCertificateSelectionCallback")
extern class ServerCertificateSelectionCallback extends cs.system.MulticastDelegate {
	function new(func:(sender:Dynamic, hostName:String)->cs.system.security.cryptography.x509certificates.X509Certificate):Void;
	function Invoke(sender:Dynamic, hostName:String):cs.system.security.cryptography.x509certificates.X509Certificate;
}
