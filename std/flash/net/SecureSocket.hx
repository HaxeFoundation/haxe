package flash.net;

@:require(flash11) extern class SecureSocket extends Socket {
	var serverCertificate(default,null) : flash.security.X509Certificate;
	var serverCertificateStatus(default,null) : String;
	function new() : Void;
	function addBinaryChainBuildingCertificate(certificate : flash.utils.ByteArray, trusted : Bool) : Void;
	static var isSupported(default,null) : Bool;
}
