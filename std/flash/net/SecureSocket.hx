package flash.net;

@:require(flash11) extern class SecureSocket extends Socket {
	var serverCertificate(default,never) : flash.security.X509Certificate;
	var serverCertificateStatus(default,never) : String;
	function new() : Void;
	function addBinaryChainBuildingCertificate(certificate : flash.utils.ByteArray, trusted : Bool) : Void;
	static var isSupported(default,never) : Bool;
}
